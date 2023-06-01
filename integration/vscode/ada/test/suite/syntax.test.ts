import * as assert from 'assert';
import { contextClients } from '../../src/extension';
import { AdaGrammarRule, AdaSyntaxCheckProvider } from '../../src/alsProtocolExtensions';
import { before, suite, test } from 'mocha';
import * as vscode from 'vscode';

suite('Syntax Check Test Suite', () => {
    before(() => {
        // eslint-disable-next-line @typescript-eslint/no-floating-promises
        vscode.window.showInformationMessage('Start all tests.');
    });
    suite('Grammer Rules Tests', () => {
        // Checking the extension is activated before running the tests.
        before(async () => {
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            const ext: vscode.Extension<any> | undefined =
                vscode.extensions.getExtension('AdaCore.ada');
            if (ext !== undefined) {
                if (!ext.isActive) {
                    await ext.activate();
                }
            }
        });
        // the test function
        async function testRule(
            rule: AdaGrammarRule,
            false_statement: string,
            true_statement: string
        ) {
            await contextClients.adaClient.onReady();
            const syntaxProvider = new AdaSyntaxCheckProvider(contextClients.adaClient, [rule]);
            let result = await syntaxProvider.sendCheckSyntaxRequest(true_statement);
            assert.deepStrictEqual(result, undefined);
            result = await syntaxProvider.sendCheckSyntaxRequest(false_statement);
            assert.deepStrictEqual(result, 'Invalid Syntax');
        }
        // the test cases
        test('Id Rule', async () => {
            await testRule(AdaGrammarRule.Defining_Id_Rule, 'package Foo', 'Foo');
        });
        test('Id List Rule', async () => {
            await testRule(AdaGrammarRule.Defining_Id_List_Rule, 'package Foo, Bar', 'Foo, Bar');
        });
        test('Def Name Rule', async () => {
            await testRule(AdaGrammarRule.Defining_Name_Rule, 'access Foo', 'Foo');
        });
        test('Param Spec Rule', async () => {
            await testRule(AdaGrammarRule.Param_Spec_Rule, 'package Foo : Bar', 'Foo : Bar');
        });
        test('Expr Rule', async () => {
            await testRule(AdaGrammarRule.Expr_Rule, 'package a + b', 'a + b');
        });
        test('Anonymous Type Rule', async () => {
            await testRule(AdaGrammarRule.Anonymous_Type_Rule, 'type access Foo', 'access Foo');
        });
        test('Subtype Id Rule', async () => {
            await testRule(AdaGrammarRule.Subtype_Indication_Rule, 'type Foo.Bar', 'Foo.Bar');
        });
    });
});
