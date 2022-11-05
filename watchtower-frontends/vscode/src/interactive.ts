import * as Elm from "node-elm-compiler";
import * as path from "path";
import * as fs from "fs";

export function generate(docs, callback) {
  const compiled_elm_js = fs
    .readFileSync(path.join(__dirname, "generate.js"))
    .toString();

  // execute generator to get a new Elm File
  run_generator(
    path.join(__dirname, "interactive", "src"),
    "Generate",
    compiled_elm_js,
    docs,
    () => {
      // We know that our generator generates one Elm file called "Live.elm"
      const compiled_interactive_js = Elm.compileToStringSync(
        ["./src/Live.elm"],
        {
          cwd: path.join(__dirname, "interactive"),
          pathToElm: path.join(__dirname, "elm-dev"),
        }
      );
      callback(compiled_interactive_js);
    }
  );
}

/* Run the generator

*/

type Warning = {
  declaration: string;
  warning: string;
};

// eval in a specifically defined scope
function scoped_eval(scope, script) {
  return Function('"use strict";' + script).bind(scope)();
}

// Run the js generated from a compiler
async function run_generator(
  output_dir: string,
  moduleName: string,
  compiled_elm_js: string,
  flags: any,
  onSuccess: any
) {
  let scope: any = {};
  scoped_eval(scope, compiled_elm_js);

  const promise = new Promise<
    { path: string; contents: string; warnings: Warning[] }[]
  >((resolve, reject) => {
    // @ts-ignore
    if (!(moduleName in scope.Elm)) {
      console.log(
        // @ts-ignore
        `Module ${moduleName} not found in compile Elm code. Available modules are: ${JSON.stringify(
          scope.Elm
        )}`
      );
      return 1;
    }

    // @ts-ignore
    const app = scope.Elm[moduleName].init({ flags: flags });
    if (app.ports.onSuccessSend) {
      app.ports.onSuccessSend.subscribe(resolve);
    }
    if (app.ports.onInfoSend) {
      app.ports.onInfoSend.subscribe((info: string) => console.log(info));
    }
    if (app.ports.onFailureSend) {
      app.ports.onFailureSend.subscribe(reject);
    }
  });

  try {
    let problemCount = 0;
    const files = await promise;
    for (const file of files) {
      const fullpath = path.join(output_dir, file.path);
      fs.mkdirSync(path.dirname(fullpath), { recursive: true });
      fs.writeFileSync(fullpath, file.contents);
      console.log("GENERATED");
      console.log(file.path);

      // if (file.warnings && file.warnings.length > 0) {
      //   console.log(format_title(`ELM CODEGEN WARNING`))
      //   console.log(`In the generated file: ${chalk.yellow(file.path)}`)
      //   for (const warn of file.warnings) {
      //     const intro = `When trying to figure out the type for ${chalk.yellow(warn.declaration)}, I ran into an issue`
      //     const warning = warn.warning.split("\n")
      //     const outro = [
      //       "",
      //       `I'm not as smart as the Elm compiler :/, but we're ${chalk.yellow(
      //         "good friends"
      //       )}.  I especially get confused when there are a lot of type aliases.`,
      //       `If you need to, try using ${chalk.cyan("Elm.withType")} to tell me what the type should be!`,
      //     ]
      //     console.log(format_block([intro, ""].concat(warning).concat(outro)))
      //   }
      //   problemCount = problemCount + 1
      // }
    }
    onSuccess();
  } catch (errors: { title: string; description: string }[] | any) {
    console.error(errors.title);
    console.error(errors.description);
    return 1;
  }
}
