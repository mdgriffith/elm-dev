export const connect = (app: any) => {
    // Listen for messages from Elm
    app.ports?.resetWindowScroll?.subscribe?.(() => {
        window.scrollTo(0, 0);
    });
};
