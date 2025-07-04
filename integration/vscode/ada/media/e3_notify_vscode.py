"""
The purpose of this module is to report e3-testsuite events to the Ada & SPARK VS Code
extension.

It provides a callback to be used with the e3-testsuite notification system via the
--notify-events CLI argument. See the documentation of the
`e3.testsuite.event_notifications` module for more information about that system.

Upon receiving e3-testsuite events, the callback prints them to stdout, as JSON objects
of a specific structure expected by the Ada & SPARK VS Code extension. That structure is
defined by the `TestsuiteNotification` type in `e3TestsuiteNotifications.ts`.
"""

import json
import os
import sys
from typing import Literal, NotRequired, TypedDict

from e3.testsuite import Testsuite
from e3.testsuite import event_notifications as EN
from e3.testsuite.event_notifications import TestNotification

NotificationType = Literal["queue", "start", "result", "end"]


class Event(TypedDict):
    kind: NotificationType
    testName: str
    resultPath: NotRequired[str]


def callback(e: TestNotification):
    if isinstance(e, EN.TestQueueNotification):
        v = Event(kind="queue", testName=e.test_name)
    elif isinstance(e, EN.TestStartNotification):
        v = Event(kind="start", testName=e.test_name)
    elif isinstance(e, EN.TestResultNotification):
        v = Event(
            kind="result", testName=e.test_name, resultPath=e.yaml_result_filename
        )
    elif isinstance(e, EN.TestEndNotification):
        v = Event(kind="end", testName=e.test_name)
    else:
        v = None

    if v is not None:
        json.dump(v, sys.stdout)
        sys.stdout.write(os.linesep)
        sys.stdout.flush()


def init_callback(ts: Testsuite):
    return callback
