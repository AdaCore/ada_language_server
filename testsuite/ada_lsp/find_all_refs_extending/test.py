"""test that a references call finds all references to an enumeration
literal in the context of extending projects."""

from drivers.pylsp import ALSLanguageClient, test


@test()
async def test_called_by(lsp: ALSLanguageClient) -> None:
    # Send a didChangeConfiguration notification to load the extending project
    lsp.didChangeConfig({"projectFile": "extending.gpr"})

    # Send a didOpen for main.adb
    common_ads = lsp.didOpenFile("common/common.ads")

    # Send a textDocument/prepareCallHierarchy request and verify the result
    res1 = await lsp.references(common_ads, 3, 21)
    lsp.assertLocationsList(
        res1,
        [
            ("common.ads", 3),
            ("common.ads", 5),
            ("extending.adb", 5),
            ("program.adb", 6),
        ],
    )

    # Now load the non-extending project
    lsp.didChangeConfig({"projectFile": "program/program.gpr"})

    # And verify the result
    res1 = await lsp.references(common_ads, 3, 21)
    lsp.assertLocationsList(
        res1,
        [
            ("common.ads", 3),
            ("common.ads", 5),
            ("program.adb", 6),
        ],
    )
