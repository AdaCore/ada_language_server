"""
Test that ALS initial memory footprint does not exceed a
given amount of memory when runtime indexing is
disabled.
"""

from drivers.pylsp import (
    ALSLanguageClient,
    ALSClientServerConfig,
    test,
    get_ALS_memory_footprint,
)
import os


async def check_memory_footprint(
    lsp: ALSLanguageClient,
    footprint_lower_bound,
    footprint_upper_bound,
):
    """
    This checks that the ALS memory footprint is within the given bounds.
    The bounds should be expresed in MB.
    """
    # Send a didOpen for main.adb
    lsp.didOpenFile("main.adb")

    # Wait for indexing
    await lsp.awaitIndexingEnd()

    # Check the memory footprint in MB (psutil returns memory info in Bytes): verify
    # that the initial footprint is within expected bounds
    mem_footprint = get_ALS_memory_footprint()

    assert footprint_lower_bound < mem_footprint < footprint_upper_bound, (
        f"Initial memory footprint should be between {footprint_lower_bound}MB "
        + f"and {footprint_upper_bound}MB. Actual memory is: {mem_footprint}MB"
    )


@test(
    config=ALSClientServerConfig(
        [
            os.environ.get("ALS", "ada_language_server"),
            "--tracefile",
            "./traces_with_runtime_indexing.cfg",
        ]
    )
)
async def test_with_indexing(lsp: ALSLanguageClient) -> None:
    await check_memory_footprint(
        lsp, footprint_lower_bound=550, footprint_upper_bound=650
    )


@test(
    config=ALSClientServerConfig(
        [
            os.environ.get("ALS", "ada_language_server"),
            "--tracefile",
            "./traces_without_runtime_indexing.cfg",
        ]
    )
)
async def test_without_indexing(lsp: ALSLanguageClient) -> None:
    await check_memory_footprint(
        lsp, footprint_lower_bound=50, footprint_upper_bound=150
    )
