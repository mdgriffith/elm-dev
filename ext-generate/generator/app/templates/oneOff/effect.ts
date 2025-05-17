export const connect = (app: any) => {
    // Listen for messages from Elm
    app.ports?.{{name_decapitalized}}?.subscribe?.((message: any) => {
        console.log("{{name_decapitalized}}", message);
    });
};
