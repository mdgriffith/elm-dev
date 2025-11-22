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
            private _childOutgoingUnsubs: Array<(payload: any) => void> = [];
            private _mainOutgoingUnsub: ((payload: any) => void) | null = null;

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
                    start.call(container);
                    const Elm = (container as any).Elm || {};
                    const candidate = this._findElmInitModule(Elm);
                    if (!candidate || typeof (candidate as any).init !== "function") {
                        throw new Error("No Elm module with an init function found");
                    }
                    this._app = (candidate as any).init({
                        node: container,
                        flags: {}
                    });

                    // Wire child <-> main ports
                    this._wirePorts();
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
                // Unsubscribe any bridge callbacks registered on main app
                // eslint-disable-next-line @typescript-eslint/no-explicit-any
                const mainApp: any = (window as any).__elmDevMain;
                if (mainApp && mainApp.ports) {
                    if (this._mainOutgoingUnsub && mainApp.ports.interactivePropertyUpdated && typeof mainApp.ports.interactivePropertyUpdated.unsubscribe === "function") {
                        try { mainApp.ports.interactivePropertyUpdated.unsubscribe(this._mainOutgoingUnsub); } catch { /* ignore */ }
                    }
                }
                this._childOutgoingUnsubs = [];
                this._mainOutgoingUnsub = null;
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

            private _wirePorts() {
                if (!this._app || !this._filePath) return;
                // eslint-disable-next-line @typescript-eslint/no-explicit-any
                const mainApp: any = (window as any).__elmDevMain;
                console.log("MAIN APP", mainApp);
                if (!mainApp || !mainApp.ports) return;

                // Child -> Main: subscribe to all child's controlsUpdated_* and forward with filepath
                if (mainApp.ports.onControlsUpdated && typeof mainApp.ports.onControlsUpdated.send === "function") {
                    const ports = (this._app.ports ?? {}) as Record<string, any>;
                    for (const name of Object.keys(ports).filter((k) => k.startsWith("controlsUpdated") && typeof ports[k]?.subscribe === "function")) {
                        console.log(
                            "CONTROLS UPDATED",
                            name,
                            ports[name]
                        )

                        const forwardToMain = (controls: any) => {
                            console.log(
                                "FORWARD TO MAIN",
                                controls
                            )
                            try {
                                mainApp.ports.onControlsUpdated.send({ filepath: this._filePath, controls });
                            } catch (e) {
                                console.error("Failed to send onControlsUpdated to main app", e);
                            }
                        };
                        ports[name].subscribe(forwardToMain);
                        this._childOutgoingUnsubs.push(forwardToMain);
                    }
                }

                // Main -> Child: subscribe to main's interactivePropertyUpdated and send to child's propertyUpdated_*
                const forwardToChild = (msg: any) => {
                    try {
                        if (!msg || msg.filepath !== this._filePath) return;
                        const ports = (this._app.ports ?? {}) as Record<string, any>;
                        for (const name of Object.keys(ports).filter((k) => k.startsWith("propertyUpdated") && typeof ports[k]?.send === "function")) {
                            const childIn = ports[name];
                            if (childIn && typeof childIn.send === "function") {
                                childIn.send({ name: msg.key, value: msg.value });
                            }
                        }
                    } catch (e) {
                        console.error("Failed to forward interactivePropertyUpdated to child", e);
                    }
                };
                if (mainApp.ports.interactivePropertyUpdated && typeof mainApp.ports.interactivePropertyUpdated.subscribe === "function") {
                    mainApp.ports.interactivePropertyUpdated.subscribe(forwardToChild);
                    this._mainOutgoingUnsub = forwardToChild;
                }
            }

            private _findElmInitModule(root: any): any | null {
                const visited = new Set<any>();
                const search = (obj: any): any | null => {
                    if (!obj || typeof obj !== "object" || visited.has(obj)) return null;
                    visited.add(obj);
                    if (typeof obj.init === "function") return obj;
                    for (const key of Object.keys(obj)) {
                        const child = (obj as any)[key];
                        if (child && typeof child === "object" && /^[A-Z]/.test(key)) {
                            const found = search(child);
                            if (found) return found;
                        }
                    }
                    return null;
                };
                return search(root);
            }
        }
    );
}

