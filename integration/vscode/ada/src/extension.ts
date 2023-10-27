/* eslint-disable @typescript-eslint/no-unsafe-call */
/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
----------------------------------------------------------------------------*/

import * as vscode from 'vscode';

import { MESSAGE } from 'triple-beam';
import { Middleware } from 'vscode-languageclient/node';
import winston, { format, transports } from 'winston';
import Transport from 'winston-transport';
import { ExtensionState } from './ExtensionState';
import { ALSClientFeatures } from './alsClientFeatures';
import { alsCommandExecutor } from './alsExecuteCommand';
import { registerCommands } from './commands';
import { initializeTestView } from './gnattest';
import {
    TERMINAL_ENV_SETTING_NAME,
    assertSupportedEnvironments,
    getEvaluatedTerminalEnv,
    startedInDebugMode,
} from './helpers';

const ADA_CONTEXT = 'ADA_PROJECT_CONTEXT';

/**
 * A global object encapsulating extension state. This includes the Ada and GPR
 * LSP clients and other state used to provide tasks, commands and other
 * functionality.
 */
export let adaExtState: ExtensionState;

/**
 * The `vscode.OutputChannel` that hosts messages from the extension. This is
 * different from the channels associated with the language servers.
 */
export let mainOutputChannel: vscode.OutputChannel;

/**
 * A Winston logger object associated with the main output channel that allows
 * logging messages in a uniform format.
 */
export const logger: winston.Logger = winston.createLogger({
    format: format.combine(
        // Include a timestamp
        format.timestamp({
            format: 'YYYY-MM-DD HH:mm:ss.SSS',
        }),
        // Annotate log messages with a label
        format.label({ label: 'Ada Extension' }),
        // Pad message levels for alignment
        format.padLevels(),
        // Include a stack trace for logged Error objects
        format.errors({ stack: true }),
        // Perform printf-style %s,%d replacements
        format.splat()
    ),
});

/**
 * This is a custom Winston transport that forwards logged messages onto a given
 * `vscode.OutputChannel`.
 */
class VSCodeOutputChannelTransport extends Transport {
    outputChannel: vscode.OutputChannel;

    constructor(channel: vscode.OutputChannel, opts?: Transport.TransportStreamOptions) {
        super(opts);
        this.outputChannel = channel;
    }

    /**
     * This implementation is based on the Winston documentation for custom transports.
     *
     * @param info - the log entry info object
     * @param next - a callback
     */
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public log(info: any, next: () => void) {
        setImmediate(() => {
            this.emit('logged', info);
        });

        /*
         * The formatted message is stored under the 'message' symbol in the info object.
         */
        // eslint-disable-next-line max-len
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-unsafe-member-access
        this.outputChannel.appendLine(info[MESSAGE]);

        if (next) {
            next();
        }
    }
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    setUpLogging(context);

    logger.info('Starting Ada extension');

    try {
        await activateExtension(context);
    } catch (error) {
        logger.error('Error while starting Ada extension. ', error);
        throw error;
    }

    logger.info('Finished starting Ada extension');
}

async function activateExtension(context: vscode.ExtensionContext) {
    assertSupportedEnvironments(logger);

    // Log the environment that the extension (and all VS Code) will be using
    const customEnv = getEvaluatedTerminalEnv();
    if (customEnv && Object.keys(customEnv).length > 0) {
        logger.info(`Custom environment variables from ${TERMINAL_ENV_SETTING_NAME}`);
        for (const varName in customEnv) {
            const varValue = customEnv[varName];
            logger.info(`${varName}=${varValue ?? '<null>'}`);
        }
    } else {
        logger.debug('No custom environment variables set in %s', TERMINAL_ENV_SETTING_NAME);
    }

    // Create the Ada and GPR clients.
    adaExtState = new ExtensionState(context);
    context.subscriptions.push(adaExtState);

    const alsMiddleware: Middleware = {
        executeCommand: alsCommandExecutor(adaExtState.adaClient),
    };
    adaExtState.adaClient.clientOptions.middleware = alsMiddleware;
    adaExtState.adaClient.registerFeature(new ALSClientFeatures());

    await adaExtState.start();

    context.subscriptions.push(
        vscode.workspace.onDidChangeConfiguration(adaExtState.configChanged)
    );

    /**
     * Register commands first so that commands such as displaying the extension
     * Output become available even if the language servers fail to start.
     */
    registerCommands(context, adaExtState);

    await vscode.commands.executeCommand('setContext', ADA_CONTEXT, true);

    await initializeTestView(context, adaExtState);

    /**
     * This can display a dialog to the User so don't wait on the result.
     */
    void vscode.commands.executeCommand('ada.addMissingDirsToWorkspace', true);
}

function setUpLogging(context: vscode.ExtensionContext) {
    // Create an output channel for the extension. There are dedicated channels
    // for the Ada and Gpr language servers, and this one is a general channel
    // for non-LSP features of the extension.
    mainOutputChannel = vscode.window.createOutputChannel('Ada Extension');

    /*
     * This is a printing formatter that converts log entries to a string. It
     * used both for logging to the output channel and to the console.
     */
    const printfFormatter = format.printf((info) => {
        // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
        return `${info.timestamp} [${info.label}] ${info.level.toUpperCase()} ${info.message} ${
            // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
            info.stack ?? ''
        }`;
    });

    /*
     * Add a transport to direct log messages to the main
     * `vscode.OutputChannel`. Set the log level to 'info' to include 'error',
     * 'warn' and 'info'. See winston documentation for log levels:
     * https://github.com/winstonjs/winston#logging-levels
     *
     * TODO consider making the log level configurable through a verbosity
     * extension setting
     */
    logger.add(
        new VSCodeOutputChannelTransport(mainOutputChannel, {
            format: printfFormatter,
            level: 'info',
        })
    );

    /**
     * Set logging level according to configuration
     */
    updateLogLevel();
    /**
     * Listen to configuration changes and update the transport level
     */
    context.subscriptions.push(
        vscode.workspace.onDidChangeConfiguration((e) => {
            if (e.affectsConfiguration('ada.trace.server')) {
                updateLogLevel();
            }
        })
    );

    if (startedInDebugMode()) {
        // In debug mode, print log messages to the console with colors. Use
        // level 'debug' for more verbosity.
        logger.add(
            new transports.Console({
                format: format.combine(
                    printfFormatter,
                    // Colorization must be applied after the finalizing printf formatter
                    format.colorize({ all: true })
                ),
                level: 'debug',
            })
        );
    }

    function updateLogLevel() {
        /**
         * Decide the log level from configuration.
         */
        const adaTraceServer: 'off' | 'messages' | 'verbose' =
            vscode.workspace.getConfiguration('ada').get('trace.server') ?? 'off';
        const logLevel: 'info' | 'debug' = adaTraceServer == 'off' ? 'info' : 'debug';
        logger.transports.forEach((t) => (t.level = logLevel));
        logger.info('Setting log level to: ' + logLevel);
    }
}

export async function deactivate() {
    await vscode.commands.executeCommand('setContext', ADA_CONTEXT, undefined);
}
