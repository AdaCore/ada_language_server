"""
The goal of this test is to check that when a .als.json file exist at the root of the
workspace, it is automatically loaded.

To test that we create a unique p1.gpr at the root. If there was no .als.json present,
p1.gpr would be loaded automatically. It's a failure sentinel.

Then we create a .als.json that sets projectFile="non-root/p2.gpr". If .als.json is
loaded, then p2.gpr is our success sentinel.

Then the test queries for the current project to determine the success or failure.
"""

from pytest_lsp import LanguageClient
from drivers.pylsp import URI, assertEqual, getCurrentProject, test


@test()
async def func(lsp: LanguageClient) -> None:
    response = await lsp.workspace_execute_command_async(getCurrentProject())
    assert response
    assertEqual(response, URI("non-root/p2.gpr"))
