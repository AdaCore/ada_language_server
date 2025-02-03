
# Refactoring Tools

## Available Refactorings

<!-- no toc -->
* [Named Parameters](#named-parameters)
* [Add Parameter](#add-parameter)
* [Remove Parameter](#remove-parameter)
* [Move Parameter](#move-parameter)
* [Change Parameter Mode](#change-parameter-mode)
* [Change Parameter Type](#change-parameter-type)
* [Change Parameter Default Value](#change-parameter-default-value)
* [Extract Subprogram](#extract-subprogram)
* [Pull Up Declaration](#pull-up-declaration)
* [Suppress Separate](#suppress-separate)
* [Introduce Parameter](#introduce-parameter)
* [Replace Type](#replace-type)
* [Auto Import](#auto-import)
* [Sort Dependencies](#sort-dependencies)

## Named Parameters

**Command name:** `als-named-parameters`

* Adds the formal parameter name of each actual parameter in the subprogram call.

See `/source/ada/lsp-ada_handlers-named_parameters_commands.ads` in [ALS repository](https://github.com/AdaCore/ada_language_server).

Demo source is `named_parameters//` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![named_parameters](https://user-images.githubusercontent.com/22893717/166944482-cfebab94-adf8-4de5-9018-8415be94f8f5.gif)


## Add Parameter

**Command name:** `als-refactor-add-parameters`

* Adds a new parameter to a subprogram.
* All subprogram specs are updated.
* Only parameters with correct syntax are accepted.
* The new parameter type is infered when only the name is provided.
* The new parameter location is infered from the cursor position.

See `src/lal_refactor-subprogram_signature.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `add_parameter//` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![add_parameter](https://user-images.githubusercontent.com/22893717/166926928-f6f5243c-6008-435f-9c2f-40aec0517936.gif)

## Remove Parameter

**Command name:** `als-refactor-remove-parameters`

* Removes a parameter of a subprogram.
* All subprogram specs are updated.
* Actual parameter is removed from the subprogram calls.

See `src/lal_refactor-subprogram_signature-remove_parameter.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `remove_parameter/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![remove parameter](https://user-images.githubusercontent.com/22893717/166926891-d621fb59-8524-4ba8-abfc-74c12fed2adf.gif)

## Move Parameter

**Command name:** `als-refactor-move-parameters`

* Moves a parameter backward and forward within a subprogram spec.
* All subprogram specs are updated.
* Actual parameter are moved in the subprogram calls when needed.

See `src/lal_refactor-subprogram_signature.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `move_parameter/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![move_parameter](https://user-images.githubusercontent.com/22893717/166927234-ba038012-25be-476e-bd29-d85c10a5b2d3.gif)

## Change Parameter Mode

**Command name:** `als-refactor-change-parameter-mode`

* Changes the parameter mode within a subprogram spec.
* All subprogram specs are updated.

See `src/lal_refactor-subprogram_signature.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `change_parameter_mode/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![change_parameter_mode](https://user-images.githubusercontent.com/22893717/166927346-cbaa9789-eb44-44df-8a9a-c2b770d3a93e.gif)

## Change Parameter Type

**Command name:** `als-refactor-change_parameters_type`

* Changes the parameter subtype indication within a subprogram spec.
* Only subtype indications with correct syntax are accepted.
* All subprogram specs are updated.

See `src/lal_refactor-subprogram_signature-change_parameters_type.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `change_parameter_type/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![change_parameter_type](https://user-images.githubusercontent.com/22893717/166927382-b04c5415-dc3e-49e1-9ef3-2840579447d8.gif)

## Change Parameter Default Value

**Command name:** `als-refactor-change_parameters_default_value`

* Changes the parameter default value expression within a subprogram spec.
* Only default value expressions with correct syntax are accepted.
* All subprogram specs are updated.

See `src/lal_refactor-subprogram_signature-change_parameters_default_value.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `change_parameter_default_value/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![change_parameter_default_value](https://user-images.githubusercontent.com/22893717/166927617-f6f33bc4-d660-44ce-b836-bf02b839887e.gif)

## Extract Subprogram

**Command name:** `als-refactor-extract-subprogram`

* Extracts statements to a new subprogram.
* The new subprogram is created in the nearest declarative part.
* Local declarations of for loop and exception handlers are passed to the extracted subprogram as new parameters.
* Extract function is available is the last statment is either an assignment or a return statment.

See `src/lal_refactor-extract_subprogram.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `extract_subprogram/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![extract_subprogram](https://user-images.githubusercontent.com/22893717/166927664-c61af27f-a446-4e3a-acbe-71b7fa88e925.gif)

## Pull Up Declaration

**Command name:** `als-refactor-pull_up_declaration`

* Moves a declaration and its dependent declarations to their parent declarative part.
* When pulling up a subprogram, object declaration are not pulled up. Instead, they're added as formal parameters to the subprogram specification and as actual parameters to the subprogram calls.

See `src/lal_refactor-pull_up_declaration.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `pull_up_declaration/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![pull_up_declaration](https://user-images.githubusercontent.com/22893717/166927695-e6b9e016-1374-4aa5-9640-60aaf1a4b7fe.gif)

## Suppress Separate

**Command name:** `als-suppress-separate`

* Moves a separate subunit to it's stub in the parent package.
* Use clauses in the separate subunit are moved to the subprogram's declarative part to avoid namespace collisions.
* .bak is added to the separate subunit source filename.

See `src/lal_refactor-suppress_separate.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `suppress_separate/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![suppress_separate](https://user-images.githubusercontent.com/22893717/166927780-441fdb3f-271f-4f69-99ff-367e8eef301e.gif)

## Introduce Parameter

**Command name:** `als-refactor-introduce-parameter`

* Introduces a formal parameter based on an object declaration or expression inside a subprogram.
* All references of the object declaration or expression are replaced by the introduced parameter.
* The user must mannually fix the calls to the subprogram that was refactored by addings the corresponding actual parameter.

See `src/lal_refactor-introduce_parameter.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `introduce_parameter/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![introduce parameter](https://user-images.githubusercontent.com/22893717/181477996-564a1365-33df-4227-bb82-e9ed802b4ed0.gif)

## Replace Type

**Command name:** `als-refactor-replace-type`

* Replaces a type in the intire project by another type provided by the user.

See `src/lal_refactor-replace_type.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `replace_type/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![replace type](https://user-images.githubusercontent.com/22893717/217803466-ae5500fe-a071-4fe9-a669-24cd9c82917a.gif)

## Auto Import

**Command name:** `als-auto-import`

* For an unresolved name, suggests all packages that can be imported and prefix to be added so that that the name gets resolved.

[Source](https://github.com/AdaCore/lal-refactor/blob/edge/src/lal_refactor-auto_import.adb` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `auto_import/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![auto import](https://user-images.githubusercontent.com/22893717/217804710-e686ef22-227b-4e81-8bb1-1f218e5709df.gif)

## Sort Dependencies

**Command name:** `als-refactor-sort_dependencies`

* Sorts all with and use clauses and their associated pragmas.

See `src/lal_refactor-sort_dependencies.ads` in [LAL Refactor repository](https://github.com/AdaCore/lal-refactor).

Demo source is `sort_dependencies/` in [Code Samples](https://github.com/AdaCore/ada_language_server/blob/master/integration/vscode/Code%20Samples/refactoring_demos/).

![replace type](https://user-images.githubusercontent.com/22893717/217805066-ee69e6d6-4c9e-4075-8eb6-1fca7793c428.gif)
