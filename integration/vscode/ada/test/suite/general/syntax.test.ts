import * as assert from 'assert';
import { adaExtState } from '../../../src/extension';
import { AdaGrammarRule, AdaSyntaxCheckProvider } from '../../../src/alsProtocolExtensions';
import { suite, test } from 'mocha';
import { activate } from '../utils';

suite('Syntax Check Test Suite', function () {
    this.beforeAll(async () => {
        await activate();
    });
    suite('Grammer Rules Tests', () => {
        // the test function
        async function testRule(
            rule: AdaGrammarRule,
            false_statement: string,
            true_statement: string
        ) {
            const syntaxProvider = new AdaSyntaxCheckProvider(adaExtState.adaClient, [rule]);
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
