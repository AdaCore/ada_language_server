from drivers.pylsp import test
from pytest_lsp import LanguageClient


@test()
async def func(lsp: LanguageClient) -> None:
    # This test aims to check that the nested property "onTypeFormatting.indentOnly" is
    # correctly parsed in the .als.json config file.
    #
    # Currently it is difficult to check this because currently there are no custom
    # requests allowing us to query the ALS for the currently loaded settings.
    #
    # TODO implement the check by opening a document and inserting a line break and
    # checking if we get an automatic indentation.
    pass
