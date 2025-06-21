export default function include() {
    customElements.define(
        "elm-embedded",
        class extends HTMLElement {
            private _app: any = null;
            private _baseUrl: string | null = null;
            private _filepath: string | null = null;

            static get observedAttributes() {
                return ["base-url", "filepath"];
            }

            constructor() {
                super();
                this.attachShadow({ mode: 'open' });
            }

            connectedCallback() {
                this._baseUrl = this.getAttribute("base-url");
                this._filepath = this.getAttribute("filepath");
                this._loadAndMountElm();
            }

            attributeChangedCallback(name: string, oldValue: string, newValue: string) {
                if (name === "base-url" && newValue !== oldValue) {
                    this._baseUrl = newValue;
                    this._loadAndMountElm();
                } else if (name === "filepath" && newValue !== oldValue) {
                    this._filepath = newValue;
                    this._loadAndMountElm();
                }
            }

            private async _loadAndMountElm() {
                if (!this._baseUrl || !this._filepath) {
                    this._showError("Missing required attributes: base-url and filepath");
                    return;
                }

                try {
                    // Clean up existing app if it exists
                    this._cleanup();

                    // Create a container for the Elm app
                    const container = document.createElement('div');
                    container.id = 'elm-app-container';
                    this.shadowRoot!.appendChild(container);

                    // Fetch the compiled Elm code from the elm-dev server
                    const url = `${this._baseUrl}/interactive?file=${encodeURIComponent(this._filepath)}`;
                    const response = await fetch(url);

                    if (!response.ok) {
                        throw new Error(`Failed to fetch Elm code: ${response.status} ${response.statusText}`);
                    }

                    const compiledCode = await response.text();

                    // Create a script element to execute the compiled code
                    const script = document.createElement('script');
                    script.textContent = compiledCode;

                    // Create a temporary container to execute the script
                    const tempContainer = document.createElement('div');
                    tempContainer.appendChild(script);
                    document.head.appendChild(tempContainer);

                    // Execute the script to define the Elm object
                    const Elm = (window as any).Elm || {};

                    // Initialize the Elm app
                    if (Elm.Main && Elm.Main.init) {
                        this._app = Elm.Main.init({
                            node: container,
                            flags: {}
                        });
                    } else {
                        throw new Error("Elm.Main.init not found in compiled code");
                    }

                    // Clean up the temporary script
                    document.head.removeChild(tempContainer);

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

                // Clear the shadow root
                if (this.shadowRoot) {
                    this.shadowRoot.innerHTML = '';
                }
            }

            private _showError(message: string) {
                if (this.shadowRoot) {
                    this.shadowRoot.innerHTML = `
                        <div style="
                            color: red; 
                            padding: 1rem; 
                            border: 1px solid red; 
                            border-radius: 4px;
                            background-color: #fff5f5;
                            font-family: monospace;
                        ">
                            <strong>Elm-embedded Error:</strong> ${message}
                        </div>
                    `;
                }
            }

            disconnectedCallback() {
                this._cleanup();
            }
        }
    );
} 