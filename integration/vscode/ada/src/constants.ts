/**
 * Identifier for a hidden command used for building and running a project main.
 * The command accepts a parameter which is the URI of the main source file.
 * It is triggered by CodeLenses provided by the extension.
 *
 * @see {@link buildAndRunSpecifiedMain}
 */
export const CMD_BUILD_AND_RUN_MAIN = 'ada.buildAndRunMain';

/**
 * Identifier for a hidden command used for building and debugging a project main.
 * The command accepts a parameter which is the URI of the main source file.
 * It is triggered by CodeLenses provided by the extension.
 *
 * @see {@link buildAndDebugSpecifiedMain}
 */
export const CMD_BUILD_AND_DEBUG_MAIN = 'ada.buildAndDebugMain';

/**
 * Identifier for a hidden command used for building and running a project main,
 * using GNATemulator.
 * The command accepts a parameter which is the URI of the main source file.
 * It is triggered by CodeLenses provided by the extension.
 *
 * @see {@link buildAndRunMainWithGNATemulator}
 */
export const CMD_BUILD_AND_RUN_GNATEMULATOR = 'ada.buildAndRunGNATemulator';

/**
 * Identifier for a hidden command used for building and debugging a project main,
 * using GNATemulator.
 * The command accepts a parameter which is the URI of the main source file.
 * It is triggered by CodeLenses provided by the extension.
 *
 * @see {@link buildAndDebugSpecifiedMain}
 */
export const CMD_BUILD_AND_DEBUG_GNATEMULATOR = 'ada.buildAndDebugGNATemulator';

/**
 * Identifier for a hidden command that returns an array of strings constituting
 * the -P and -X project and scenario arguments.
 */
export const CMD_GPR_PROJECT_ARGS = 'ada.gprProjectArgs';

/**
 * Identifier for a hidden command that returns a string referencing the current
 * project.  That string is either `"$\{config:ada.projectFile\}"` if that
 * setting is configured, or otherwise the full path to the project file
 * returned from a query to the
 */
export const CMD_GET_PROJECT_FILE = 'ada.getProjectFile';

export const CMD_SPARK_LIMIT_SUBP_ARG = 'ada.spark.limitSubpArg';
export const CMD_SPARK_LIMIT_REGION_ARG = 'ada.spark.limitRegionArg';
export const CMD_SPARK_PROVE_SUBP = 'ada.spark.proveSubprogram';
export const CMD_SPARK_ASK_OPTIONS = 'ada.spark.askGNATproveOptions';
export const CMD_SPARK_CURRENT_GNATPROVE_OPTIONS = 'ada.spark.gnatproveOptions';

/**
 * Identifier for the command that opens the extension's user's guide.
 */
export const CMD_OPEN_USERS_GUIDE = 'ada.openUsersGuide';

/**
 * Identifier for the command that shows the extension's output in the Output panel.
 */
export const CMD_SHOW_EXTENSION_LOGS = 'ada.showExtensionOutput';

/**
 * Identifier for the command that shows the output of the ALS for Ada in the Output panel.
 */
export const CMD_SHOW_ADA_LS_OUTPUT = 'ada.showAdaLSOutput';

/**
 * Identifier for the command that shows the output of the ALS for GPR in the Output panel.
 */
export const CMD_SHOW_GPR_LS_OUTPUT = 'ada.showGprLSOutput';

/**
 * Identifier for the command that reloads the currently loaded project on server-side.
 */
export const CMD_RELOAD_PROJECT = 'als-reload-project';

/**
 * Identifier for the command that restarts all the language servers spawned by the extension
 * (Ada and GPR).
 */
export const CMD_RESTART_LANG_SERVERS = 'ada.restartLanguageServers';

/**
 * Live doc URL of the Ada & SPARK VS Code extension User's Guide.
 */
export const VSCODE_UG_LIVE_DOC_URL =
    'https://docs.adacore.com/live/wave/als/html/als-doc/index.html';
