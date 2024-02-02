
let spawn = require('cross-spawn');


// opts ->
//  { cwd: elmPackageJsonPath,
//    env: process.env,
//  }
export function executeExpectJson(scriptPath, args, opts) {
    return new Promise(function (resolve, reject) {
        function finish() {

            var proc = spawn(scriptPath, args, opts);
            var jsonStr = '';
            var stderrStr = '';


            proc.stdout.on('data', function (data) {
                jsonStr += data;
            });

            proc.stderr.on('data', function (data) {
                stderrStr += data;
            });

            proc.on('close', function (code) {
                if (stderrStr !== '') {
                    reject(stderrStr);
                } else if (code !== 0) {
                    reject('Finding test interfaces failed, exiting with code ' + code);
                }
                var parsed;
                
                try {
                    if (jsonStr.trim() == ""){
                        parsed = null;
                    } else {
                        parsed = JSON.parse(jsonStr);
                    }
                } catch (err) {
                    reject('Received invalid JSON from test interface search: ' + err);
                }


                return resolve(parsed);
            });
        }

        return finish();
    });
}
