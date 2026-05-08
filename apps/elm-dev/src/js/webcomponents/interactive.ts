export default function include() {
    customElements.define(
        "elm-dev",
        class extends HTMLElement {
            private _app: any = null;
            private _code: string | null = null;
            private _data: any = null;

            static get observedAttributes() {
                return ["code", "data"];
            }

            attributeChangedCallback(name: string, oldValue: string, newValue: string) {
                if (name === "code" && newValue !== oldValue) {
                    this._code = newValue;
                    this._mountElm();
                } else if (name === "data" && newValue !== oldValue) {
                    this._data = newValue;
                    this._updateData();
                }
            }

            private _mountElm() {
                if (!this._code) return;

                // Clean up existing app if it exists
                if (this._app) {
                    this._app.ports.dataUpdated.unsubscribe();
                    this.innerHTML = "";
                }

                try {
                    const Elm = {};
                    const start = new Function(this._code);
                    start.call(Elm);

                    // We know that the compiled code will have Main.init
                    // @ts-expect-error
                    this._app = Elm.Main.init({
                        node: this,
                        flags: this._data
                    })


                    // If we have data, send it to the app
                    if (this._data) {
                        this._updateData();
                    }
                } catch (error: any) {
                    console.error("Failed to mount Elm widget:", error);
                    this.innerHTML = `<div style="color: red">Failed to mount Elm widget: ${error.message}</div>`;
                }
            }

            private _updateData() {
                if (this._app && this._app.ports && this._app.ports.dataUpdated) {
                    try {
                        this._app.ports.dataUpdated.send(this._data);
                    } catch (error: any) {
                        console.error("Failed to update data:", error);
                    }
                }
            }
        }
    );
}
