import { fetch } from "@tauri-apps/plugin-http";

export default function include() {
    customElements.define(
        "elm-playground",
        class extends HTMLElement {
            private _app: any = null;
            private _elmSource: string | null = null;
            private _baseurl: string | null = null;
            private _projectRoot: string | null = null;
            private _filePath: string | null = null;
            private _lastCompiled: string | null = null;

            static get observedAttributes() {
                return ["elm-source", "baseurl", "project-root", "filepath"];
            }

            constructor() {
                super();
            }

            connectedCallback() {
                this._elmSource = this.getAttribute("elm-source");
                this._baseurl = this.getAttribute("baseurl");
                this._projectRoot = this.getAttribute("project-root");
                this._filePath = this.getAttribute("filepath");
                void this._compileAndMount();
            }

            attributeChangedCallback(name: string, _oldValue: string, newValue: string) {
                if (name === "elm-source") {
                    this._elmSource = newValue;
                } else if (name === "baseurl") {
                    this._baseurl = newValue;
                } else if (name === "project-root") {
                    this._projectRoot = newValue;
                } else if (name === "filepath") {
                    this._filePath = newValue;
                }
                void this._compileAndMount();
            }

            private async _compileAndMount() {
                if (!this._elmSource || !this._baseurl || !this._projectRoot || !this._filePath) {
                    this._showError(`Missing required attributes: elm-source, baseurl, project-root, filepath`);
                    return;
                }

                // Avoid recompiling identical source
                const cacheKey = `${this._baseurl}::${this._projectRoot}::${this._filePath}::${this._elmSource}`;
                if (this._lastCompiled === cacheKey) return;
                this._lastCompiled = cacheKey;

                try {
                    this._cleanup();
                    const container = document.createElement("div");
                    container.style.cssText = "width: 100%; height: 100%;";
                    this.appendChild(container);

                    console.log("INTERACTIVE", this._projectRoot, this._filePath);
                    // Compile source via dev server (POST body = elm source)
                    const url = new URL(`${this._baseurl}/dev/interactive/compile`);
                    url.searchParams.set("dir", this._projectRoot);
                    url.searchParams.set("file", this._filePath);

                    const res = await fetch(url.toString(), { method: "POST", headers: { "Content-Type": "text/plain; charset=utf-8" }, body: this._elmSource });
                    if (!res.ok) {
                        const text = (await (res as any).text?.()) ?? "";
                        throw new Error(`Compile failed: ${text || `${res}`}`);
                    }
                    const js = await (res as any).text?.();
                    if (!js || js.length === 0) {
                        throw new Error("Empty JS response from compiler");
                    }

                    // Execute compiled JS and initialize Elm app
                    const start = new Function(js);
                    start.call(window);
                    const Elm = (window as any).Elm || {};
                    const candidate =
                        (Elm.Main && Elm.Main.init)
                            ? Elm.Main
                            : Object.values(Elm).find((m: any) => m && typeof (m as any).init === "function");
                    if (!candidate || typeof (candidate as any).init !== "function") {
                        throw new Error("No Elm module with an init function found");
                    }
                    this._app = (candidate as any).init({
                        node: container,
                        flags: {}
                    });
                } catch (err: any) {
                    console.error("elm-playground error:", err);
                    this._showError(err?.message ?? String(err));
                }
            }

            private _cleanup() {
                if (this._app) {
                    if (this._app.ports) {
                        Object.values(this._app.ports).forEach((port: any) => {
                            if (port && typeof port.unsubscribe === "function") {
                                port.unsubscribe();
                            }
                        });
                    }
                    this._app = null;
                }
                this.innerHTML = "";
            }

            private _showError(message: string) {
                const errorDiv = document.createElement("div");
                errorDiv.style.color = "red";
                errorDiv.style.padding = "1rem";
                errorDiv.style.border = "1px solid red";
                errorDiv.style.borderRadius = "4px";
                errorDiv.style.backgroundColor = "#fff5f5";
                errorDiv.style.fontFamily = "monospace";
                const strong = document.createElement("strong");
                strong.textContent = "Elm-playground Error: ";
                errorDiv.appendChild(strong);
                errorDiv.appendChild(document.createTextNode(message));
                this.appendChild(errorDiv);
            }

            disconnectedCallback() {
                this._cleanup();
            }
        }
    );
}


