import { fetch } from "@tauri-apps/plugin-http";

type Params = {
    baseUrl: string;
    filepath: string;
    cwd: string;
}

export default function include() {
    customElements.define(
        "elm-embedded",
        class extends HTMLElement {
            private _app: any = null;
            private _baseUrl: string | null = null;
            private _filepath: string | null = null;
            private _cwd: string | null = null;
            private _loaded: Params | null = null;

            static get observedAttributes() {
                return ["base-url", "filepath", "cwd"];
            }

            constructor() {
                super();
                // Don't use shadow DOM to avoid DOM manipulation issues
            }

            connectedCallback() {
                this._baseUrl = this.getAttribute("base-url");
                this._filepath = this.getAttribute("filepath");
                this._cwd = this.getAttribute("cwd");
                this._loadAndMountElm();
            }

            attributeChangedCallback(name: string, oldValue: string, newValue: string) {
                if (name === "base-url" && newValue !== oldValue) {
                    this._baseUrl = newValue;
                    this._loadAndMountElm();
                } else if (name === "filepath" && newValue !== oldValue) {
                    this._filepath = newValue;
                    this._loadAndMountElm();
                } else if (name === "cwd" && newValue !== oldValue) {
                    this._cwd = newValue;
                    this._loadAndMountElm();
                }
            }

            private async _loadAndMountElm() {
                console.log("LOADING ELM", { baseUrl: this._baseUrl, filepath: this._filepath, cwd: this._cwd });
                if (!this._baseUrl || !this._filepath || !this._cwd) {
                    this._showError(`Missing required attributes: base-url, filepath, and cwd:\n   base:${this._baseUrl}\n   filepath:${this._filepath}\n   cwd:${this._cwd}`);
                    return;
                }

                if (this._loaded && this._loaded.baseUrl === this._baseUrl && this._loaded.filepath === this._filepath && this._loaded.cwd === this._cwd) {
                    console.log("ALREADY LOADED or LOADING", this._loaded);
                    return;
                }
                this._loaded = {
                    baseUrl: this._baseUrl,
                    filepath: this._filepath,
                    cwd: this._cwd
                };

                try {
                    // Clean up existing app if it exists
                    this._cleanup();

                    // Create a container for the Elm app
                    const container = document.createElement('div');
                    container.id = 'elm-app-container';
                    container.style.cssText = 'width: 100%; height: 100%;';

                    // Add the container to the element
                    this.appendChild(container);

                    // Build the URL with query parameters (dev JS endpoint)
                    const url = new URL(`${this._baseUrl}/dev/interactive`);
                    url.searchParams.set('file', this._filepath);
                    if (this._cwd) {
                        url.searchParams.set('dir', this._cwd);
                    }

                    // Use Tauri HTTP (no browser fallback)
                    const response = await fetch(url.toString(), { method: "GET" });
                    if (!response.ok) {
                        throw new Error(`Failed to fetch Elm code: ${response.status} ${response.statusText}`);
                    }
                    const textResult = await response.text();

                    if (!textResult || textResult.length === 0) {
                        throw new Error("Empty response when fetching compiled Elm JS");
                    }

                    console.log("FETCHED ELM CODE", textResult);

                    const parsed = JSON.parse(textResult);
                    console.log("PARSED", parsed);
                    for (const file of parsed.generated) {
                        console.log(file.contents);
                    }

                    // Execute the compiled code in a custom scope
                    // const scope: any = {};
                    const start = new Function(textResult);
                    start.call(window);

                    // Get the Elm object from the scope
                    const EmbeddedElm = (window as any).Elm || {};

                    // Initialize the Elm app with the container
                    const candidate =
                        (EmbeddedElm.Interactive && EmbeddedElm.Interactive.init)
                            ? EmbeddedElm.Interactive
                            : Object.values(EmbeddedElm).find((m: any) => m && typeof (m as any).init === "function");
                    if (!candidate || typeof (candidate as any).init !== "function") {
                        throw new Error("No Elm module with an init function was found in compiled code");
                    }
                    console.log("INITIALIZING ELM APP", { container, module: candidate });
                    this._app = (candidate as any).init({
                        node: container,
                        flags: {}
                    });

                } catch (error: any) {
                    console.error("Failed to load and mount Elm widget:", error);
                    this._showError(`Failed to load Elm widget: ${error.message}`);
                }
            }

            private _cleanup() {
                if (this._app) {
                    // Clean up any ports if they exist
                    if (this._app.ports) {
                        Object.values(this._app.ports).forEach((port: any) => {
                            if (port && typeof port.unsubscribe === 'function') {
                                port.unsubscribe();
                            }
                        });
                    }
                    this._app = null;
                }

                // Clear the element
                this.innerHTML = '';
            }

            private _showError(message: string) {
                const errorDiv = document.createElement('div');
                errorDiv.style.color = 'red';
                errorDiv.style.padding = '1rem';
                errorDiv.style.border = '1px solid red';
                errorDiv.style.borderRadius = '4px';
                errorDiv.style.backgroundColor = '#fff5f5';
                errorDiv.style.fontFamily = 'monospace';

                const strong = document.createElement('strong');
                strong.textContent = 'Elm-embedded Error: ';
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