export default function include(options: { mountId: string }) {
  Object.defineProperty(Element.prototype, "__getParentClientRect", {
    get() {
      return this.parentNode.getBoundingClientRect();
    },
  });

  Object.defineProperty(Element.prototype, "__getWindowSize", {
    get() {
      return { width: window.innerWidth, height: window.innerHeight };
    },
  });

  customElements.define(
    "elm-portal",
    class extends HTMLElement {
      private _targetNode: any;
      // Base custom element stuff
      connectedCallback() {
        this._targetNode = document.createElement("div");
        if (this.target) {
          this.target.appendChild(this._targetNode);
        } else {
          // If there is no place to mount the elements, we want to throw immediately.
          // This node should always be present, even at app startup.
          throw new Error(
            `There is no place to mount elements that are using elm-portal.  I was looking for #${options.mountId}, but didn't find anything.`
          );
        }
      }

      get target() {
        return document.getElementById(options.mountId);
      }

      disconnectedCallback() {
        if (this.target) {
          this.target.removeChild(this._targetNode);
        }
      }

      // Re-implementations of HTMLElement functions
      get childNodes() {
        return this._targetNode.childNodes;
      }

      replaceData(...args: any[]) {
        return this._targetNode.replaceData(...args);
      }

      removeChild(...args: any[]) {
        return this._targetNode.removeChild(...args);
      }

      insertBefore(...args: any[]) {
        return this._targetNode.insertBefore(...args);
      }
      appendChild(node: any) {
        // To cooperate with the Elm runtime
        requestAnimationFrame(() => {
          return this._targetNode.appendChild(node);
        });
        return node;
      }
    }
  );
}
