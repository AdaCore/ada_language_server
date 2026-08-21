testjson=$1
dir=`dirname $testjson`
basename=` basename $testjson  | rev | cut -b 6- | rev  `
./.obj/tester/tester-run $testjson
python testsuite/drivers/trace_to_test.py /home/setton/.als/in.txt /home/setton/.als/out.txt $dir > $dir/$basename.test

$ (cd .github/skills/_shared && node_modules/.bin/tsx src/cli.ts \
    create-and-test --schema bank_statement.schema.json \
    --input mixed_financial_docs.pdf --output /tmp/results/ \
    --reuse --ephemeral)
[CREATE] analyzer_id=bank_statement.schema_62465204
[CREATE] bank_statement.schema_62465204 ready
[ANALYZE] mixed_financial_docs.pdf -> /tmp/results/mixed_financial_docs.json
[CLEANUP] delete analyzer bank_statement.schema_62465204

[SUMMARY]
category: (single)  (1 document)
  field                                    fill rate   avg conf
  accountHolder                            100.0%      0.962
  accountNumber                            100.0%      0.962
  beginningBalance                         100.0%      0.902
  endingBalance                            100.0%      0.902
  ...
  transactions[].deposit                    15.4%      0.853
lowest-confidence fields:
  0.587  transactions[].date  (mixed_financial_docs)

  $ (cd .github/skills/_shared && node_modules/.bin/tsx src/cli.ts \
    extract-layout --input mixed_financial_docs.pdf --output /tmp/layout/)
[RUN ] mixed_financial_docs.pdf -> /tmp/layout//mixed_financial_docs.layout.{json,md}
[DONE] 1 ok, 0 failed

declarative_item ::= simple_declarative_item
  | typed_string_declaration
  | package_declaration

simple_declarative_item ::= variable_declaration
  | typed_variable_declaration
  | attribute_declaration
  | case_construction
  | empty_declaration

empty_declaration ::= 'null' ;

package_declaration ::= package_spec | package_renaming | package_extension

package_spec ::=
  'package' <package_>simple_name 'is'
     { simple_declarative_item }
  'end' package_identifier ;

package_renaming ::=
  'package' <package_>simple_name 'renames'
        <project_>simple_name.package_identifier ;

package_extension ::=
  'package' <package_>simple_name 'extends'
        <project_>simple_name.package_identifier 'is'
     { simple_declarative_item }
  'end' package_identifier ;
