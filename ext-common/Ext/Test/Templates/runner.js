/* Minimal Node runner that executes the compiled Elm Runner and prints JSON */
const fs = require('fs');
const path = require('path');

async function run() {
    try {
        const compiledPath = process.argv[2];
        if (!compiledPath) {
            console.error('Usage: node runner.js <compiled.js>');
            process.exit(1);
        }
        const code = fs.readFileSync(compiledPath, 'utf8');
        const start = new Function(code + '\n;return this;');
        const scope = start.call({});
        const Elm = scope.Elm || global.Elm;
        if (!Elm || !Elm.Runner) {
            console.error('Runner module not found in compiled code');
            process.exit(1);
        }
        let done = false;
        const app = Elm.Runner.init({});
        app.ports.results.subscribe((json) => {
            if (!done) {
                done = true;
                console.log(json);
                process.exit(0);
            }
        });
        setTimeout(() => {
            if (!done) {
                console.error('Timeout running tests');
                process.exit(1);
            }
        }, 30000);
    } catch (e) {
        console.error(e && e.stack || String(e));
        process.exit(1);
    }
}

run();



