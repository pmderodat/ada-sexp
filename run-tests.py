#! /usr/bin/env python

"""
e3.testsuite-based testsuite for Sexp.

Just execute this script as a main to run the testsuite.
"""

from __future__ import absolute_import, print_function

import difflib
import json
import os.path
import pprint
import sys

from e3.fs import sync_tree
from e3.os.process import Run
import e3.testsuite
import e3.testsuite.driver
from e3.testsuite.process import check_call
from e3.testsuite.result import TestStatus


TESTSUITE_ROOT = os.path.dirname(os.path.abspath(__file__))


class TestDriver(e3.testsuite.driver.TestDriver):
    """
    Common code for test drivers.
    """

    def push_status(self, log, status=TestStatus.FAIL):
        """
        Log content to the result, set test status and push the result.

        :param None|str log: Optional content to log.
        """
        if log:
            self.result.log += log
        self.result.set_status(status)
        self.push_result()
        return True

    def push_success(self):
        """
        Shortcut to set test status to PASS and to push the result.
        """
        return self.push_status(None, TestStatus.PASS)

    def push_for_diff(self, error_label, a, b, fromfile, tofile):
        """
        Shortcut to set test status based on whether "a" and "b" are different.

        :param str error_label: Label to include in the log in case of
            non-empty diff.

        Arguments `a`, `b`, `fromfile` and `tofile` have the same semantics as
        difflib.unified_diff's corresponding arguments.
        """
        diff = list(difflib.unified_diff(
            a=a, b=b, fromfile=fromfile, tofile=tofile, lineterm=''))

        if diff:
            self.push_status(
                '{}:\n{}'.format(error_label, '\n'.join(diff)))
            raise e3.testsuite.TestAbort


class DecoderTestDriver(TestDriver):
    """
    Test driver to run the "sexp_decode" program.

    This test driver runs the "sexp_decode" program on the test-provided
    "input.sexp" input file and checks that its output is equivalent to the
    "output" entry in the test.yaml file.

    If the test.yaml file contains an "error" entry, expect that
    "sexp_decode" exits with a non-zero status code. If "error" is not True,
    it must be a string: this checks that t it emits the provided error
    message.
    """

    decoder_program = 'obj-checkers/sexp-test_decode'
    encoder_program = 'obj-checkers/sexp-test_encode'
    input_file = 'input.sexp'

    def add_test(self, dag):
        self.add_fragment(dag, 'run')

    def _run_decoder(self, filename):
        with open(filename, 'rb') as f:
            return Run([os.path.join(TESTSUITE_ROOT, self.decoder_program)],
                       input=f)

    def _run_encoder(self, filename):
        with open(filename, 'rb') as f:
            return Run([os.path.join(TESTSUITE_ROOT, self.encoder_program)],
                       input=f)

    def run(self, previous_values):
        work_dir = self.test_env['working_dir']

        # Copy all test material to the working directory
        sync_tree(self.test_env['test_dir'], work_dir)

        # Get the expected JSON document, or the expected error message
        expected_error = None
        try:
            expected_json = self.test_env['output']
        except KeyError:
            expected_json = None
            expected_error = self.test_env['error']

        # Run the decoder with the S-Expression content on the standard input
        p = self._run_decoder(os.path.join(work_dir, self.input_file))

        # If we expected an error, make sure we have the expected one
        if expected_error:
            if p.status == 0:
                return self.push_status(
                    'Error expected, but parsing succeeded')
            elif (
                expected_error is not True and
                p.out.strip() != expected_error
            ):
                return self.push_status('Unexpected error:\n{}'.format(p.out))
            else:
                return self.push_success()

        # Otherwise, make sure the decoder succeeded and produced the expected
        # result.
        if p.status != 0:
            return self.push_status(
                'Decoder exitted with error status ({}):'
                '\n{}'.format(p.status, p.out))

        json_text_output = p.out

        try:
            p_output_json = json.loads(p.out)
        except ValueError as exc:
            return self.push_status(
                'Cannot parse the output JSON document:'
                '\n{}'
                '\nOutput was: {}'.format(exc, p.out))

        if p_output_json != expected_json:
            p_output_pretty = pprint.pformat(p_output_json).splitlines()
            expected_pretty = pprint.pformat(expected_json).splitlines()

            self.push_for_diff('Unexpected JSON output for the decoder',
                               p_output_pretty, expected_pretty,
                               'decoder output', 'expected output')

        # Now, try to reformat the S-Expression from the JSON output and make
        # sure it produces something that the decoder can re-parse the same
        # way.

        # Put the JSON input in a file (to ease post-mortem analysis)
        input_json_file = os.path.join(work_dir, 'input.json')
        with open(input_json_file, 'w') as f:
            f.write(json_text_output)

        # Run the encoder on it
        p = self._run_encoder(input_json_file)
        if p.status != 0:
            return self.push_status('Encoder failed: {}'.format(p.out))

        # Put the resulting S-Expression document in a file
        input_sexp_file = os.path.join(work_dir, 'second-input.sexp')
        with open(input_sexp_file, 'w') as f:
            f.write(p.out)

        # Run the decoder on it
        p = self._run_decoder(input_sexp_file)
        if p.status != 0:
            return self.push_status(
                'Second decoder exitted with error status ({}):'
                '\n{}'.format(p.status, p.out))

        if json_text_output != p.out:
            self.push_for_diff('Unexpected second decoder output',
                               p.out.splitlines(),
                               json_text_output.splitlines(),
                               'second decoder output',
                               'first decoder output')

        return self.push_success()


class RunProgramTestDriver(TestDriver):
    """
    Test driver to build and run a program that uses the Sexp project.

    This test driver builds the test-provided "main.adb" program and then run
    it, checking that its status code is 0 and that its output matches the
    content of the test-provided "test.out" file.
    """

    main_file = 'main.adb'
    expected_output_file = 'test.out'

    def add_test(self, dag):
        self.add_fragment(dag, 'build')
        self.add_fragment(dag, 'run', after=['build'])

    def build(self, previous_values):
        # Copy all test material to the working directory
        sync_tree(self.test_env['test_dir'], self.test_env['working_dir'])

        # Create a project file to build the test program
        project_file = os.path.join(self.test_env['working_dir'], 'p.gpr')
        with open(project_file, 'w') as f:
            f.write("""
                with "sexp";

                project P is
                    for Main use ("main.adb");
                    for Object_Dir use "obj";

                    package Compiler is
                        for Default_Switches ("Ada") use ("-g");
                    end Compiler;
                end P;
            """)

        # Now build it
        check_call(self, ['gprbuild', '-P', project_file, '-p'])

        return True

    def run(self, previous_values):
        if not previous_values['build']:
            return

        # Run the test program
        p = check_call(self, ['obj/main'])

        # Check that its output matches expectations
        with open(os.path.join(self.test_env['test_dir'],
                               self.expected_output_file), 'r') as f:
            expected_output = f.read().splitlines()

        self.push_for_diff(
            'Unexpected test program output',
            p.out.splitlines(),
            expected_output,
            '{} output'.format(self.main_file),
            self.expected_output_file)

        return self.push_success()


class Testsuite(e3.testsuite.Testsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {'decoder': DecoderTestDriver,
               'run-program': RunProgramTestDriver}

    @property
    def default_driver(self):
        return 'decoder'

    def add_options(self):
        self.main.argument_parser.add_argument(
            '--no-auto-path',
            action='store_true',
            help='Do not automatically add this repository to'
                 ' GPR_PROJECT_PATH. Adding it is the default for developer'
                 ' convenience.')

    def set_up(self):
        if not self.main.args.no_auto_path:
            old_value = os.environ.get('GPR_PROJECT_PATH', '')
            if old_value:
                new_value = '{}{}{}'.format(TESTSUITE_ROOT, os.path.pathsep,
                                            old_value)
            else:
                new_value = TESTSUITE_ROOT
            os.environ['GPR_PROJECT_PATH'] = new_value


if __name__ == '__main__':
    # Some test YAML documents create big recursions in the YAML parser
    sys.setrecursionlimit(10000)

    suite = Testsuite(os.path.dirname(__file__))
    suite.testsuite_main()

    # Display statistics about test results: number of tests per status
    stats = [(str(name).split('.')[1], count)
             for name, count in suite.test_status_counters.items()
             if count]
    for name, count in sorted(stats):
        print('{: <8} {}'.format(name + ':', count))
