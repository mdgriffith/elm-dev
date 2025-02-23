import { Editor } from "@tiptap/core";
import StarterKit from "@tiptap/starter-kit";
import Link from "@tiptap/extension-link";

const toStyles = (styles: any) => {
  return {
    class: styles ? styles.class : undefined,
    style: styles ? styles.style : undefined,
  };
};

export default function include() {
  customElements.define(
    "tiptap-control",
    class extends HTMLElement {
      private _editorControl: any;
      // Base custom element stuff
      connectedCallback() {
        this.onclick = this.handleClick.bind(this); // Bind the click handler
      }
      handleClick() {
        const editorNode = this.getEditor();
        // @ts-ignore
        if (editorNode && editorNode.editor && this._editorControl) {
          // @ts-ignore
          const editor = editorNode.editor;
          switch (this._editorControl.cmd) {
            case "toggle-heading":
              editor
                .chain()
                .focus()
                .toggleHeading({ level: this._editorControl.heading })
                .run();
              break;
            case "toggle-bold":
              editor.chain().focus().toggleBold().run();
              break;
            case "toggle-italic":
              editor.chain().focus().toggleItalic().run();
              break;
            case "toggle-code":
              editor.chain().focus().toggleCode().run();
              break;
            case "toggle-codeblock":
              editor.chain().focus().toggleCodeBlock().run();
              break;
            case "toggle-bullet-list":
              editor.chain().focus().toggleBulletList().run();
              break;
            case "toggle-numbered-list":
              editor.chain().focus().toggleOrderedList().run();
              break;
            default:
              break;
          }
        }
      }

      getEditor() {
        const parent = this.closest(".tiptap-editor-container");
        if (parent) {
          return parent.querySelector("tiptap-editor");
        }
        return null;
      }

      set editorControl(val) {
        if ("cmd" in val && typeof val.cmd === "string") {
          this._editorControl = val;
        }
      }
      get editorControl() {
        return this._editorControl;
      }
    }
  );

  customElements.define(
    "tiptap-editor",
    class extends HTMLElement {
      _editor: any;
      private _editorNode: any;
      private _content: any;
      private _cssClasses: any;
      private _cssStyle: any;
      private _editable: any;
      private _elementStyles: any;
      // Base custom element stuff
      connectedCallback() {
        const self = this;
        this._editor = new Editor({
          element: this,
          extensions: [
            StarterKit.configure({
              heading: {
                levels: [1, 2],
                HTMLAttributes: toStyles(this._elementStyles.heading),
              },
              paragraph: {
                HTMLAttributes: toStyles(this._elementStyles.paragraph),
              },
              blockquote: {
                HTMLAttributes: toStyles(this._elementStyles.blockquote),
              },
              bold: {
                HTMLAttributes: toStyles(this._elementStyles.bold),
              },
              code: {
                HTMLAttributes: toStyles(this._elementStyles.code),
              },
              codeBlock: {
                HTMLAttributes: toStyles(this._elementStyles.codeBlock),
              },
              hardBreak: {
                HTMLAttributes: toStyles(this._elementStyles.hardBreak),
              },
              horizontalRule: {
                HTMLAttributes: toStyles(this._elementStyles.horizontalRule),
              },
              italic: {
                HTMLAttributes: toStyles(this._elementStyles.italic),
              },
              orderedList: {
                HTMLAttributes: toStyles(this._elementStyles.orderedList),
              },
              listItem: {
                HTMLAttributes: toStyles(this._elementStyles.listItem),
              },
              strike: {
                HTMLAttributes: toStyles(this._elementStyles.strike),
              },
            }),
            Link.configure({
              openOnClick: true,
              autolink: true,
              linkOnPaste: true,
            }),
          ],
          content: this._content,
          editable: this._editable,
          editorProps: {
            attributes: {
              class: this._cssClasses ? this._cssClasses : undefined,
              style: this._cssStyle ? this._cssStyle : undefined,
            },
          },

          onUpdate: ({ editor }) => {
            const contents = editor.getJSON();
            console.log(contents);
            const event = new CustomEvent("editor-updated", {
              detail: self,
            });
            self.dispatchEvent(event);
          },
          onFocus: ({ editor }) => {
            self.dispatchEvent(new Event("editor-focused"));
          },
          onBlur: ({ editor }) => {
            self.dispatchEvent(new Event("editor-blurred"));
          },
          onSelectionUpdate: (options) => {
            console.log(options);
            self.dispatchEvent(new Event("editor-selection-updated"));
          },
          onDestroy: (_) => {
            self.dispatchEvent(new Event("editor-destroyed"));
          },
        });
      }

      set elementStyles(val) {
        this._elementStyles = val;
      }
      get elementStyles(): boolean {
        return this._elementStyles;
      }

      set editable(val) {
        this._editable = val;
      }
      get editable(): boolean {
        return this._editable;
      }

      set cssClass(val) {
        this._cssClasses = val;
      }

      get cssClass(): string {
        return this._cssClasses ? this._cssClasses : "";
      }

      set cssStyle(val) {
        this._cssStyle = val;
      }

      get cssStyle(): string {
        return this._cssStyle;
      }

      get json() {
        return this._editor.getJSON();
      }

      get editor() {
        return this._editor;
      }
      set content(val) {
        this._content = val;
      }
      get content() {
        return this._content;
      }
    }
  );
}
