"""
The goal of this test is to check that comments are supported in .als.json files

To test that, we create a non-root/p2.gpr project file and configure it in a .als.json
with comments. We can determine if the config loading was successful (i.e. comments are
supported) by querying for the current project.
"""

from drivers.pylsp import URI, ALSLanguageClient, assertEqual, test


@test()
async def func(lsp: ALSLanguageClient) -> None:
    response = await lsp.getCurrentProject()
    assertEqual(response, URI("non-root/p2.gpr"))
