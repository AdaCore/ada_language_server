import * as assert from 'assert';
import { before } from 'mocha';
import { contextClients } from '../../src/extension';
import { AdaGrammarRule, AdaSyntaxCheckProvider } from '../../src/alsProtocolExtensions';

import * as vscode from 'vscode';

suite('Syntax Check Test Suite', () => {
    before(() => {
        // eslint-disable-next-line @typescript-eslint/no-floating-promises
        vscode.window.showInformationMessage('Start all tests.');
    });

    test('Grammer Rules Tests', async () => {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const ext: vscode.Extension<any> | undefined =
            vscode.extensions.getExtension('AdaCore.ada');
        if (ext !== undefined) {
            if (!ext.isActive) {
                await ext.activate();
            }
        }
        await contextClients.adaClient.onReady();
        let syntaxProvider = new AdaSyntaxCheckProvider(contextClients.adaClient, [
            AdaGrammarRule.Defining_Id_Rule,
        ]);
        let result = await syntaxProvider.sendCheckSyntaxRequest('Foo');
        assert.deepStrictEqual(result, undefined);
        result = await syntaxProvider.sendCheckSyntaxRequest('package Foo');
        assert.deepStrictEqual(result, 'Invalid Syntax');

        syntaxProvider = new AdaSyntaxCheckProvider(contextClients.adaClient, [
            AdaGrammarRule.Defining_Id_List_Rule,
        ]);
        result = await syntaxProvider.sendCheckSyntaxRequest('Foo, Bar');
        assert.deepStrictEqual(result, undefined);
        result = await syntaxProvider.sendCheckSyntaxRequest('package Foo, Bar');
        assert.deepStrictEqual(result, 'Invalid Syntax');

        syntaxProvider = new AdaSyntaxCheckProvider(contextClients.adaClient, [
            AdaGrammarRule.Defining_Name_Rule,
        ]);
        result = await syntaxProvider.sendCheckSyntaxRequest('Foo');
        assert.deepStrictEqual(result, undefined);
        result = await syntaxProvider.sendCheckSyntaxRequest('access Foo');
        assert.deepStrictEqual(result, 'Invalid Syntax');

        syntaxProvider = new AdaSyntaxCheckProvider(contextClients.adaClient, [
            AdaGrammarRule.Expr_Rule,
        ]);
        result = await syntaxProvider.sendCheckSyntaxRequest('a + b');
        assert.deepStrictEqual(result, undefined);
        result = await syntaxProvider.sendCheckSyntaxRequest('package a + b');
        assert.deepStrictEqual(result, 'Invalid Syntax');

        syntaxProvider = new AdaSyntaxCheckProvider(contextClients.adaClient, [
            AdaGrammarRule.Param_Spec_Rule,
        ]);
        result = await syntaxProvider.sendCheckSyntaxRequest('Foo : Bar');
        assert.deepStrictEqual(result, undefined);
        result = await syntaxProvider.sendCheckSyntaxRequest('package Foo : Bar');
        assert.deepStrictEqual(result, 'Invalid Syntax');

        syntaxProvider = new AdaSyntaxCheckProvider(contextClients.adaClient, [
            AdaGrammarRule.Anonymous_Type_Rule,
        ]);
        result = await syntaxProvider.sendCheckSyntaxRequest('access Foo');
        assert.deepStrictEqual(result, undefined);
        result = await syntaxProvider.sendCheckSyntaxRequest('type access Foo');
        assert.deepStrictEqual(result, 'Invalid Syntax');

        syntaxProvider = new AdaSyntaxCheckProvider(contextClients.adaClient, [
            AdaGrammarRule.Subtype_Indication_Rule,
        ]);
        result = await syntaxProvider.sendCheckSyntaxRequest('Foo.Bar');
        assert.deepStrictEqual(result, undefined);
        result = await syntaxProvider.sendCheckSyntaxRequest('type Foo.Bar');
        assert.deepStrictEqual(result, 'Invalid Syntax');
    });
});
