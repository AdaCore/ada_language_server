
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

## Named Parameters

* Adds the formal parameter name of each actual parameter in the subprogram call.

[Source](https://github.com/AdaCore/ada_language_server/blob/master/source/ada/lsp-ada_handlers-named_parameters_commands.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/named_parameters/)

![named_parameters](https://user-images.githubusercontent.com/22893717/166944482-cfebab94-adf8-4de5-9018-8415be94f8f5.gif)


## Add Parameter

* Adds a new parameter to a subprogram.
* All subprogram specs are updated.
* Only parameters with correct syntax are accepted.
* The new parameter type is infered when only the name is provided.
* The new parameter location is infered from the cursor position.

[Source](https://github.com/AdaCore/libadalang-tools/blob/master/src/laltools-refactor-subprogram_signature.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/add_parameter/)

![add_parameter](https://user-images.githubusercontent.com/22893717/166926928-f6f5243c-6008-435f-9c2f-40aec0517936.gif)

## Remove Parameter

* Removes a parameter of a subprogram.
* All subprogram specs are updated.
* Actual parameter is removed from the subprogram calls.

[Source](https://github.com/AdaCore/libadalang-tools/blob/master/src/laltools-refactor-subprogram_signature-remove_parameter.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/remove_parameter)

![remove parameter](https://user-images.githubusercontent.com/22893717/166926891-d621fb59-8524-4ba8-abfc-74c12fed2adf.gif)

## Move Parameter

* Moves a parameter backward and forward within a subprogram spec.
* All subprogram specs are updated.
* Actual parameter are moved in the subprogram calls when needed.

[Source](https://github.com/AdaCore/libadalang-tools/blob/master/src/laltools-refactor-subprogram_signature.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/move_parameter)

![move_parameter](https://user-images.githubusercontent.com/22893717/166927234-ba038012-25be-476e-bd29-d85c10a5b2d3.gif)

## Change Parameter Mode

* Changes the parameter mode within a subprogram spec.
* All subprogram specs are updated.

[Source](https://github.com/AdaCore/libadalang-tools/blob/master/src/laltools-refactor-subprogram_signature.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/change_parameter_mode)

![change_parameter_mode](https://user-images.githubusercontent.com/22893717/166927346-cbaa9789-eb44-44df-8a9a-c2b770d3a93e.gif)

## Change Parameter Type

* Changes the parameter subtype indication within a subprogram spec.
* Only subtype indications with correct syntax are accepted.
* All subprogram specs are updated.

[Source](https://github.com/AdaCore/libadalang-tools/blob/master/src/laltools-refactor-subprogram_signature-change_parameters_type.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/change_parameter_type)

![change_parameter_type](https://user-images.githubusercontent.com/22893717/166927382-b04c5415-dc3e-49e1-9ef3-2840579447d8.gif)

## Change Parameter Default Value

* Changes the parameter default value expression within a subprogram spec.
* Only default value expressions with correct syntax are accepted.
* All subprogram specs are updated.

[Source](https://github.com/AdaCore/libadalang-tools/blob/master/src/laltools-refactor-subprogram_signature-change_parameters_defualt_value.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/change_parameter_default_value)

![change_parameter_default_value](https://user-images.githubusercontent.com/22893717/166927617-f6f33bc4-d660-44ce-b836-bf02b839887e.gif)

## Extract Subprogram

* Extracts statements to a new subprogram.
* The new subprogram is created in the nearest declarative part.
* Local declarations of for loop and exception handlers are passed to the extracted subprogram as new parameters.
* Extract function is available is the last statment is either an assignment or a return statment.

[Source](https://github.com/AdaCore/libadalang-tools/blob/master/src/laltools-refactor-extract_subprogram.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/extract_subprogram)

![extract_subprogram](https://user-images.githubusercontent.com/22893717/166927664-c61af27f-a446-4e3a-acbe-71b7fa88e925.gif)

## Pull Up Declaration

* Moves a declaration and its dependent declarations to their parent declarative part.
* When pulling up a subprogram, object declaration are not pulled up. Instead, they're added as formal parameters to the subprogram specification and as actual parameters to the subprogram calls.

[Source](https://github.com/AdaCore/libadalang-tools/blob/master/src/laltools-refactor-pull_up_declaration.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/pull_up_declaration)

![pull_up_declaration](https://user-images.githubusercontent.com/22893717/166927695-e6b9e016-1374-4aa5-9640-60aaf1a4b7fe.gif)

## Suppress Separate

* Moves a separate subunit to it's stub in the parent package.
* Use clauses in the separate subunit are moved to the subprogram's declarative part to avoid namespace collisions.
* .bak is added to the separate subunit source filename.

[Source](https://github.com/AdaCore/libadalang-tools/blob/master/src/laltools-refactor-suppress_separate.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/suppress_separate)

![suppress_separate](https://user-images.githubusercontent.com/22893717/166927780-441fdb3f-271f-4f69-99ff-367e8eef301e.gif)

## Introduce Parameter

* Introduces a formal parameter based on an object declaration or expression inside a subprogram.
* All references of the object declaration or expression are replaced by the introduced parameter.
* The user must mannually fix the calls to the subprogram that was refactored by addings the corresponding actual parameter.

[Source](https://github.com/AdaCore/libadalang-tools/blob/master/src/laltools-refactor-introduce_parameter.ads)

[Demo Source](../integration/vscode/Code%20Samples/refactoring_demos/introduce_parameter)

![introduce parameter](https://user-images.githubusercontent.com/22893717/181477996-564a1365-33df-4227-bb82-e9ed802b4ed0.gif)

