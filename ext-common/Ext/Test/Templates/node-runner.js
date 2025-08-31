/* Node supervisor/worker runner for Elm Runner
   Reads JSON from stdin: { flags: { seed, runs }, tests: [TestId], concurrency?: number }
   Prints raw list of per-test reports to stdout: Report[] (as emitted by Elm)
*/
const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');
const os = require('os');

/* {{COMPILED_ELM}} */

function parseInputFromStdin() {
    return new Promise((resolve, reject) => {
        const chunks = [];
        process.stdin.on('data', (c) => chunks.push(c));
        process.stdin.on('end', () => {
            try {
                const input = JSON.parse(Buffer.concat(chunks).toString('utf8'));
                resolve(input);
            } catch (e) {
                reject(e);
            }
        });
        process.stdin.on('error', reject);
        process.stdin.resume();
    });
}

if (isMainThread) {
    (async () => {
        try {
            const input = await parseInputFromStdin();
            const flags = input.flags || { seed: 0, runs: 100 };
            const tests = Array.isArray(input.tests) ? input.tests : [];
            const concurrency = Math.max(
                1,
                Math.min((os.cpus() && os.cpus().length) || 1, 8)
            );

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
                                        reports.push(msg.report);
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

