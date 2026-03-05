import { spawn } from "node:child_process";

function run(command, args, { timeoutMs = 5000, allowFailure = false } = {}) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, { shell: true, stdio: "inherit" });
    let settled = false;

    const finish = (fn) => (value) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      fn(value);
    };

    const timer = setTimeout(() => {
      try {
        child.kill("SIGKILL");
      } catch (_error) {
        // ignore kill failures
      }
      if (allowFailure) {
        resolve();
      } else {
        reject(new Error(`Timed out: ${command} ${args.join(" ")}`));
      }
    }, timeoutMs);

    child.on("error", finish((err) => {
      if (allowFailure) resolve();
      else reject(err);
    }));

    child.on("close", finish((code) => {
      if (code === 0 || allowFailure) {
        resolve();
      } else {
        reject(new Error(`Command failed (${code}): ${command} ${args.join(" ")}`));
      }
    }));
  });
}

await run("elm-dev", ["dev", "stop"], { timeoutMs: 3000, allowFailure: true });
await run("elm-dev", ["dev", "start"], { timeoutMs: 7000 });
await run("node", ["--test", "tests/hmr-harness.test.mjs"], { timeoutMs: 180000 });
