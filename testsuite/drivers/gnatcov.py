import glob
import logging
import os.path
import shutil

from e3.os.process import Run, quote_arg


class GNATcov(object):
    """
    Helper to compute code coverage with GNATcoverage using the testsuite.

    After initialization, the workflow for this helper is:

    * use the `decorate_run()` method to run a program that contributes to code
      coverage (i.e. that instrumented programs);
    * call the `report()` method to generate coverage reports.

    Instrumentation and trace production create artifacts in the `.obj/gnatcov`
    directory (well, except for instrumented sources themselves, which are put
    in `$OBJ_DIR/gnatcov-instr` for each instrumented project. Coverage reports
    are generated in `.obj/gnatcov/report-$FORMAT`.
    """

    def __init__(self, testsuite, covlevel='stmt'):
        """
        :param ALSTestsuite testsuite: Testsuite instance to work with.
        :param str covlevel: Coverage level to pass to GNATcoverage.
        """
        self.covlevel = covlevel
        self.sid_dir = testsuite.env.options.gnatcov

        self.temp_dir = os.path.join(testsuite.env.working_dir, 'gnatcov')
        self.traces_dir = os.path.join(self.temp_dir, 'traces')
        self.output_dir = testsuite.output_dir

        self.ensure_clean_dir(self.temp_dir)
        os.mkdir(self.traces_dir)

    @staticmethod
    def ensure_clean_dir(dirname):
        """
        If it exists, remove the ``dirname`` directory tree and create an empty
        directory instead.
        """
        if os.path.exists(dirname):
            shutil.rmtree(dirname)
        os.mkdir(dirname)

    @staticmethod
    def checked_run(argv):
        """
        Run a process with the given arguments. Log its output and raise an
        error if it fails.
        """
        p = Run(argv)
        if p.status != 0:
            logging.error('Command failed: %s',
                          ' '.join(quote_arg(arg) for arg in argv))
            logging.error('Output:\n' + p.out)
            raise RuntimeError

    def decorate_run(self, driver, kwargs):
        """
        Modify e3.os.process.Run arguments to run a test program.

        This must be called for all programs that contribute to code coverage.
        Given a test `driver`, this and return creates a modified copy of
        `kwargs` (the keyword arguments passed to e3.os.process.Run.
        """
        kwargs = dict(kwargs)

        # Unique identifier for the test, used to generate unique file names
        # for traces. This assumes that each testcase runs at most one such
        # program.
        uid = driver.test_env['test_name']
        trace_file = os.path.join(self.traces_dir, uid + '.srctrace')

        env = kwargs.setdefault('env', {})
        env['GNATCOV_TRACE_FILE'] = trace_file

        # Unless the caller wants to disable environment inheritance, only add
        # the above environment variable.
        kwargs.setdefault('ignore_environ', False)

        return kwargs

    def report(self, formats=['dhtml', 'xcov']):
        """Generate coverage reports for all given output formats."""

        # Get the list of all SID files
        sid_list = os.path.join(self.temp_dir, 'sid_files.txt')
        with open(sid_list, 'w') as f:
            for s in glob.glob(os.path.join(self.sid_dir, '*.sid')):
                f.write(s + '\n')

        # Get the list of all trace files
        traces_list = os.path.join(self.temp_dir, 'traces.txt')
        with open(traces_list, 'w') as f:
            for t in glob.glob(os.path.join(self.traces_dir, '*.srctrace')):
                f.write(t + '\n')

        # Load trace files only once, produce a checkpoint for them
        logging.info('Consolidating coverage results')
        ckpt_file = os.path.join(self.temp_dir, 'report.ckpt')
        self.checked_run(['gnatcov', 'coverage', '--level', self.covlevel,
                          '--sid', '@' + sid_list,
                          '--save-checkpoint', ckpt_file,
                          '@' + traces_list])

        # Now, generate all requested reports from this checkpoint
        logging.info('Generating coverage reports ({})'
                     .format(', '.join(sorted(formats))))
        for fmt in formats:
            report_dir = os.path.join(self.output_dir, 'coverage-' + fmt)
            self.ensure_clean_dir(report_dir)
            self.checked_run([
                'gnatcov', 'coverage',
                '--annotate', fmt,
                '--level', self.covlevel,
                '--output-dir', report_dir,
                '--checkpoint', ckpt_file])
