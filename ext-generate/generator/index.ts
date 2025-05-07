import * as path from "path";

const ElmGenerator = require("./dist/generate.js");


export type Summary = { errors: Error[] } | { generated: Generated[] };

export type Error = { title: string; description: string };
export type Generated = { outputDir: string; path: string; contents: string };


// Run a standard generator made by elm-codegen
export async function run(
    outputDir: string,
    flags: any
): Promise<Summary> {
    return new Promise((resolve, reject) => {
        // @ts-ignore
        const app = ElmGenerator.Elm.Run.init({ flags: flags });
        if (app.ports.onSuccessSend) {
            app.ports.onSuccessSend.subscribe(resolve);
        }
        if (app.ports.onInfoSend) {
            app.ports.onInfoSend.subscribe((info: string) => console.log(info));
        }
        if (app.ports.onFailureSend) {
            app.ports.onFailureSend.subscribe(reject);
        }
    })
        .then((files: any) => {
            const generated: Generated[] = [];
            for (const file of files) {
                generated.push({
                    outputDir: outputDir,
                    path: path.join(outputDir, file.path),
                    contents: file.contents,
                });
            }
            return { generated: generated };
        })
        .catch((errorList) => {
            const errors: Error[] = [];
            for (const error of errorList) {
                errors.push({
                    title: error.title as string,
                    description: error.description as string,
                });
            }

            return { errors: errors };
        });
}

// Add stdin processing
let data = '';
process.stdin.setEncoding('utf-8');
process.stdin.on('data', chunk => {
    data += chunk;
});

process.stdin.on('end', async () => {
    try {
        // console.log("elm.generate.test.json", data);
        // console.log("-----<");
        const input = JSON.parse(data);
        // console.error(input);
        if (!input.outputDir || !input.flags) {
            console.error('Input must contain "outputDir" and "flags" properties');
            process.exit(1);
        }
        const summary = await run(input.outputDir, input.flags).catch(err => {
            console.error(err);
            process.exit(1);
        });
        console.log(JSON.stringify(summary));
    } catch (err) {
        console.error('Failed to parse JSON input:', err);
        process.exit(1);
    }
});


