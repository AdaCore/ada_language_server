/**
 * This module defines a structure of notifications received from e3-testsuite.
 *
 * The Python module `e3_notify_vscode.py` is provided to e3-testsuite as an
 * event callback. It prints events on stdout as JSON objects matching the
 * structure defined here.
 */
export enum NotificationType {
    TestQueue = 'queue',
    TestStart = 'start',
    TestResult = 'result',
    TestEnd = 'end',
}

export interface AbstractNotification {
    kind: NotificationType;
    testName: string;
}

export interface TestQueue extends AbstractNotification {
    kind: NotificationType.TestQueue;
}

export interface TestStart extends AbstractNotification {
    kind: NotificationType.TestStart;
}

export interface TestResult extends AbstractNotification {
    kind: NotificationType.TestResult;
    /**
     * Path of the YAML file containing a test result
     */
    resultPath: string;
}

export interface TestEnd extends AbstractNotification {
    kind: NotificationType.TestEnd;
}

export type TestsuiteNotification = TestQueue | TestStart | TestResult | TestEnd;
