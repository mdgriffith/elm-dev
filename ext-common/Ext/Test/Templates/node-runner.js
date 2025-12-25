/* Node supervisor/worker runner for Elm Runner
   Reads JSON from stdin: { flags: { seed, runs }, tests: [TestId], concurrency?: number }
   Prints raw list of per-test reports to stdout: Report[] (as emitted by Elm)
*/
const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');
const os = require('os');

// Silence dev-mode warnings from compiled Elm during tests
const __originalWarn = console.warn;
console.warn = function () { };

/* {{COMPILED_ELM}} */

function parseInputFromStdin() {
    return new Promise((resolve, reject) => {
        const chunks = [];
        process.stdin.on('data', (c) => chunks.push(c));
        process.stdin.on('end', () => {
            try {
                const raw = JSON.parse(Buffer.concat(chunks).toString('utf8'));
                const input = validateInput(raw);
                resolve(input);
            } catch (e) {
                reject(e);
            }
        });
        process.stdin.on('error', reject);
        process.stdin.resume();
    });
}

function validateInput(raw) {
    if (!isPlainObject(raw)) {
        throw new Error('Input must be a JSON object');
    }

    const allowedTopKeys = new Set(['flags', 'tests']);
    for (const key of Object.keys(raw)) {
        if (!allowedTopKeys.has(key)) {
            throw new Error('Unexpected top-level field: ' + key);
        }
    }

    if (!('flags' in raw)) {
        throw new Error('Missing required field: flags');
    }
    if (!('tests' in raw)) {
        throw new Error('Missing required field: tests');
    }

    const flags = raw.flags;
    if (!isPlainObject(flags)) {
        throw new Error('flags must be an object');
    }
    const allowedFlagKeys = new Set(['seed', 'runs']);
    for (const key of Object.keys(flags)) {
        if (!allowedFlagKeys.has(key)) {
            throw new Error('Unexpected field in flags: ' + key);
        }
    }
    if (!Number.isFinite(flags.seed) || !Number.isInteger(flags.seed) || flags.seed < 0) {
        throw new Error('seed must be a non-negative integer');
    }
    if (!Number.isFinite(flags.runs) || !Number.isInteger(flags.runs) || flags.runs < 1) {
        throw new Error('flags.runs must be a positive integer');
    }

    const tests = raw.tests;
    if (!Array.isArray(tests)) {
        throw new Error('tests must be an array of strings');
    }
    for (let i = 0; i < tests.length; i++) {
        const v = tests[i];
        if (typeof v !== 'string' || v.length === 0) {
            throw new Error('tests[' + i + '] must be a non-empty string');
        }
    }

    return { flags: { seed: flags.seed, runs: flags.runs }, tests };
}

function isPlainObject(value) {
    return value !== null && typeof value === 'object' && !Array.isArray(value);
}

if (isMainThread) {
    (async () => {
        try {
            const input = await parseInputFromStdin();
            const flags = input.flags;
            const tests = input.tests;
            const cpuCount = (os.cpus() && os.cpus().length) || 1;
            const desiredByTests = Math.floor(tests.length / 4);
            const concurrency = Math.max(1, Math.min(cpuCount, desiredByTests));

            let nextIndex = 0;
            const reports = [];

            const workers = Array.from({ length: concurrency }, () => new Worker(__filename, { workerData: { flags } }));

            await Promise.all(
                workers.map(
                    (w) =>
                        new Promise((resolve) => {
                            const tryAssign = () => {
                                if (nextIndex < tests.length) {
                                    const id = tests[nextIndex++];
                                    w.postMessage({ type: 'run', id });
                                } else {
                                    w.postMessage({ type: 'shutdown' });
                                }
                            };

                            w.on('message', (msg) => {
                                switch (msg && msg.type) {
                                    case 'ready':
                                        tryAssign();
                                        break;
                                    case 'result':
                                        // Check if this report indicates the test was skipped because it's not a test
                                        const report = msg.report;
                                        if (!report.skippedBecauseNotATest) {
                                            reports.push(report);
                                        }
                                        // Always continue to next test, whether we added the report or not
                                        tryAssign();
                                        break;
                                    case 'error':
                                        // Errors are not summarized; print to stderr for visibility
                                        console.error('[worker error]', msg && msg.id, msg && msg.error);
                                        tryAssign();
                                        break;
                                }
                            });
                            w.on('exit', () => resolve());
                            w.on('error', () => resolve());
                        })
                )
            );

            process.stdout.write(JSON.stringify(reports));
            process.exit(0);
        } catch (e) {
            console.error(e && e.stack ? e.stack : String(e));
            process.exit(1);
        }
    })();
} else {
    try {
        const flags = (workerData && workerData.flags) || { seed: 0, runs: 100 };
        const Elm = global.Elm || (module && module.exports && module.exports.Elm);
        if (!Elm || !Elm.Runner) {
            throw new Error('Runner module not found in compiled code');
        }
        const app = Elm.Runner.init({ flags });
        parentPort.postMessage({ type: 'ready' });
        app.ports.reportSent.subscribe((payload) => {
            parentPort.postMessage({ type: 'result', report: payload });
        });
        app.ports.error.subscribe((err) => {
            parentPort.postMessage({ type: 'error', error: err });
        });
        parentPort.on('message', (msg) => {
            if (!msg) return;
            if (msg.type === 'run') {
                app.ports.runTest.send(msg.id);
            } else if (msg.type === 'shutdown') {
                process.exit(0);
            }
        });
    } catch (e) {
        parentPort.postMessage({ type: 'error', error: e && e.stack ? e.stack : String(e) });
        process.exit(1);
    }
}

