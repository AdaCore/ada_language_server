import glob
import logging
import os.path
import shutil

from e3.os.process import Run, quote_arg


class GNATcov(object):
    """
    Helper to compute code coverage with GNATcoverage using the testsuite.

    After initialization, the workflow for this helper is:

    * call the `build()` method to build instrumented programs;
    * use the `decorate_run()` method to run a program that contributes to code
      coverage (i.e. that instrumented programs);
    * call the `report()` method to generate coverage reports.

    Instrumentation and trace production create artifacts in the `.obj/gnatcov`
    directory (well, except for instrumented sources themselves, which are put
    in `$OBJ_DIR/gnatcov-instr` for each instrumented project. Coverage reports
    are generated in `.obj/gnatcov/report-$FORMAT`.
    """

    class Project(object):
        """Description of a project to cover."""
        def __init__(self, gnatcov_dir, prj, deps=[], for_coverage=True,
                     has_mains=False):
            """
            :param str gnatcov_dir: Absolute path to the directory to host
                gnatcov artifacts.
            :param str prj: Name of the project whose coverage needs to be
                analyzed.
            :param list[str] deps: Subset of `prj` dependencies for which we
                want to get coverage.
            :param bool for_coverage: Whether we want to compute code coverage
                for the sources in this project.
            :param bool has_mains: Whether this project file contains main
                units.
            """
            self.prj = prj
            self.deps = deps
            self.for_coverage = for_coverage
            self.has_mains = has_mains

            # Path to the project file to analyze, relative to the repository
            # base directory.
            self.prj_file = os.path.join('gnat', prj + '.gpr')

            # Name and absolute file for `prj`'s project extension
            self.ext_prj = 'ext_' + prj
            self.ext_prj_file = os.path.join(gnatcov_dir,
                                             self.ext_prj + '.gpr')

            # Compute the list of projects in self's closure (self included)
            # that matter for coverage.
            closure = [self]
            for d in deps:
                closure.extend(d.projects_in_closure)
            self.projects_in_closure = [p for p in closure if p.for_coverage]

    def __init__(self, testsuite, covlevel='stmt'):
        """
        :param ALSTestsuite testsuite: Testsuite instance to work with.
        :param str covlevel: Coverage level to pass to GNATcoverage.
        """
        self.covlevel = covlevel

        self.repo_base = testsuite.env.repo_base
        self.gnatcov_dir = os.path.abspath(os.path.join(
            testsuite.env.repo_base, '.obj', 'gnatcov'))
        self.traces_dir = os.path.join(self.gnatcov_dir, 'traces')

        self.projects = []
        def create_project(*args, **kwargs):
            result = self.Project(self.gnatcov_dir, *args, **kwargs)
            self.projects.append(result)
            return result

        # Projects involved in code coverage
        self.lsp = create_project('lsp')
        self.lsp_server = create_project(
            'lsp_server', deps=[self.lsp], has_mains=True)
        self.lsp_client = create_project(
            'lsp_client', deps=[self.lsp], for_coverage=False)
        self.codec_test = create_project(
            'codec_test', deps=[self.lsp_client],
            for_coverage=False, has_mains=True)
        self.main_projects = [p for p in self.projects if p.has_mains]

        # Files to contain trace files decoding data for gnatcov
        self.isi_files = []

        # We are going to instrument several programs, so make the rest of the
        # testsuite use the instrumented version.
        testsuite.env.als = os.path.join(
            self.gnatcov_dir, 'obj-lsp_server', 'ada_language_server')
        testsuite.env.codec_test = os.path.join(
            self.gnatcov_dir, 'obj-codec_test', 'codec_test')

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

    def build(self, jobs=1):
        """Build instrumented programs."""
        logging.info('Building instrumented programs')

        # Create output directories, if needed. Also remove trace files from
        # previous runs.
        if not os.path.exists(self.gnatcov_dir):
            os.mkdir(self.gnatcov_dir)
        if os.path.exists(self.traces_dir):
            shutil.rmtree(self.traces_dir)
        os.mkdir(self.traces_dir)

        # Generate project extensions to make the "gnatcov_rts_full" project
        # available everywhere.
        for p in self.projects:
            deps = ['gnatcov_rts_full'] + [d.ext_prj for d in p.deps]
            with open(p.ext_prj_file, 'w') as f:
                f.write(
                    """
                        {deps}

                        project {p.ext_prj} extends "../../{p.prj_file}" is
                            for Object_Dir use "obj-{p.prj}";
                        end {p.ext_prj};
                    """
                    .format(deps='\n'.join('with "{}";'.format(d)
                                           for d in deps),
                            p=p))

        # Instrument projects with mains: this will also instrument its
        # dependencies (see --projects below).
        for p in self.main_projects:
            isi_file = os.path.join(self.gnatcov_dir, p.prj + '.isi')
            self.checked_run(
                ['gnatcov', 'instrument', '--level', self.covlevel,
                 '-P', os.path.join(self.repo_base, p.prj_file)] +
                ['--projects={}'.format(pic.prj)
                 for pic in p.projects_in_closure] +
                ['--dump-method', 'atexit', isi_file])
            self.isi_files.append(isi_file)

        # Finally build instrumented programs. Disable style checks, as it does
        # not make sense for instrumented sources. Also disable warnings as
        # errors, as it's expected for generated code to contain issues (unused
        # WITHed packages, for instance).
        for p in self.main_projects:
            self.checked_run(
                ['gprbuild', '-P', p.ext_prj_file,
                 '-j{}'.format(jobs),
                 '-p', '--src-subdirs=gnatcov-instr'] +
                ['-XALS_WARN_ERRORS=false', '-cargs', '-gnatyN'])

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

        # Get the list of all trace files
        traces_list = os.path.join(self.gnatcov_dir, 'traces.txt')
        with open(traces_list, 'w') as f:
            for t in glob.glob(os.path.join(self.traces_dir, '*.srctrace')):
                f.write(t + '\n')

        # Load trace files only once, produce a checkpoint for them
        logging.info('Consolidating coverage results')
        ckpt_file = os.path.join(self.gnatcov_dir, 'report.ckpt')
        args = ['gnatcov', 'coverage', '--level', self.covlevel]
        for isi_file in self.isi_files:
            args += ['--isi', isi_file]
        args += ['--save-checkpoint', ckpt_file, '@' + traces_list]
        self.checked_run(args)

        # Now, generate all requested reports from this checkpoint
        logging.info('Generating coverage reports ({})'
                     .format(', '.join(sorted(formats))))
        for fmt in formats:
            output_dir = os.path.join(self.gnatcov_dir, 'report-' + fmt)
            if os.path.exists(output_dir):
                shutil.rmtree(output_dir)
            os.mkdir(output_dir)
            self.checked_run([
                'gnatcov', 'coverage',
                '--annotate', fmt,
                '--level', self.covlevel,
                '--output-dir', output_dir,
                '--checkpoint', ckpt_file])
