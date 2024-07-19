#! env python
"""This tool wraps around the e3.testsuite XUnit report import feature to customize test
naming based on the reports obtained in VS Code testing from the mocha-junit-reporter
module."""

from e3.testsuite.report.xunit import XUnitImporter, XUnitImporterApp


class XUnitImporterCustomTestNaming(XUnitImporter):
    def get_test_name(
        self,
        testsuite_name: str,
        testcase_name: str,
        classname: str | None = None,
    ) -> str:
        """Override naming scheme to ignore the testcase name because it
        duplicates the information from the testsuite name and the classname
        attribute."""
        return super().get_test_name(testsuite_name, "", classname)


class MochaJUnitImporterApp(XUnitImporterApp):

    def create_importer(self) -> XUnitImporter:
        return XUnitImporterCustomTestNaming(self.index, self.xfails)


if __name__ == "__main__":
    MochaJUnitImporterApp().run()
