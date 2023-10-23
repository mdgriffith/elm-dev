//
const flags = JSON.parse(process.argv[2]);

async function generate(elmModule) {
  const promise = new Promise((resolve, reject) => {
    // @ts-ignore
    const app = elmModule.init({ flags: flags });
    if (app.ports.onSuccessSend) {
      app.ports.onSuccessSend.subscribe((val) => {
        console.log(JSON.stringify(val));
        resolve(val);
      });
    }
    if (app.ports.onInfoSend) {
      app.ports.onInfoSend.subscribe((info) => console.log(info));
    }
    if (app.ports.onFailureSend) {
      app.ports.onFailureSend.subscribe(reject);
    }
  });

  return await promise;
}

return generate(this.Elm.Generate);
