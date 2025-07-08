#!env python
import sys
from time import sleep
from typing import Any, NotRequired, TypedDict

from e3.testsuite import Log, ParsedTest, TestResult, TestStatus
from e3.testsuite import Testsuite as Suite
from e3.testsuite.driver import TestDriver


class BareDriver(TestDriver):
    def add_test(self, dag):
        self.add_fragment(dag, "run")

    def run(self, prev: dict[str, Any], slot: int) -> None:
        self.test_env: TestEnv  # type: ignore
        sleep(2)

        for r in self.test_env.get("results", []):
            result = TestResult(
                self.test_name + ("." + r.get("name", "") if r.get("name") else ""),
                status=r["status"],
                msg=r.get("msg", ""),
            )

            for k in ("log", "out", "expected"):
                if k in r:
                    setattr(result, k, Log(r[k]))  # type: ignore

            self.push_result(result)


class TestResultSpec(TypedDict):
    name: NotRequired[str]
    status: TestStatus
    msg: NotRequired[str]
    log: NotRequired[str]
    out: NotRequired[str]
    expected: NotRequired[str]


class TestEnv(TypedDict):
    results: NotRequired[list[TestResultSpec]]


class Testsuite(Suite):
    @property
    def test_driver_map(self) -> dict[str, type[TestDriver]]:
        return {"bare": BareDriver}

    @property
    def default_driver(self) -> str:
        return "bare"

    def get_test_list(self, sublist):
        def test(name: str, env: TestEnv):
            return ParsedTest(name, None, env, ".", name)  # type: ignore

        all_tests = [
            test(
                "01-test-one-result",
                {
                    "results": [
                        {
                            "status": TestStatus.PASS,
                        }
                    ]
                },
            ),
            test(
                "02-test-multiple-results",
                {
                    "results": [
                        {
                            "name": "sub1",
                            "status": TestStatus.PASS,
                        },
                        {
                            "name": "sub2",
                            "status": TestStatus.FAIL,
                            "msg": "Failure message",
                        },
                        {
                            "name": "sub3",
                            "status": TestStatus.FAIL,
                            "msg": "Failure message",
                            "log": "Long\nExecution\nLog",
                        },
                        {
                            "status": TestStatus.PASS,
                        },
                    ]
                },
            ),
            test(
                "03-test-multiple-passing-results",
                {
                    "results": [
                        {
                            "name": "sub1",
                            "status": TestStatus.PASS,
                        },
                        {
                            "name": "sub2",
                            "status": TestStatus.PASS,
                        },
                        {
                            "name": "sub3",
                            "status": TestStatus.PASS,
                        },
                        {
                            "status": TestStatus.PASS,
                        },
                    ]
                },
            ),
            test(
                "04-test-only-passing-sub-results",
                {
                    "results": [
                        {
                            "name": "sub1",
                            "status": TestStatus.PASS,
                        },
                        {
                            "name": "sub2",
                            "status": TestStatus.PASS,
                        },
                        {
                            "name": "sub3",
                            "status": TestStatus.PASS,
                        },
                    ]
                },
            ),
            test(
                "05-test-only-sub-results-one-failing",
                {
                    "results": [
                        {
                            "name": "sub1",
                            "status": TestStatus.PASS,
                        },
                        {
                            "name": "sub2",
                            "status": TestStatus.FAIL,
                        },
                        {
                            "name": "sub3",
                            "status": TestStatus.PASS,
                        },
                    ]
                },
            ),
            test(
                "06-test-with-diff",
                {
                    "results": [
                        {
                            "status": TestStatus.FAIL,
                            "msg": "Failure short message",
                            "out": "Actual\nOutput\nText",
                            "expected": "Expected\nOutput\nText content",
                            "log": "Long\nExecution\nLog",
                        }
                    ]
                },
            ),
            test("07-test-with-no-results", {}),
        ]

        return (
            all_tests
            if len(sublist) == 0
            else [t for t in all_tests if t.test_name in sublist]
        )


if __name__ == "__main__":
    sys.exit(Testsuite().testsuite_main())
