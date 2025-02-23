import * as Clipboard from "./clipboard";
import * as LocalStorage from "./local-storage";
import * as TextSelection from "./text-selection";

// Handling data from Elm to JS
export function connect(app: any) {
  // Text selection
  app.ports?.textSelection?.subscribe?.((message: any) => {
    TextSelection.focus_and_select(message.id);
  });

  // Clipboard
  app.ports?.clipboard?.subscribe?.((message: any) => {
    Clipboard.copy(message);
  });

  // Scroll
  app.ports?.resetWindowScroll?.subscribe?.(() => {
    window.scrollTo(0, 0);
  });

  // Local Storage
  app.ports?.localStorage?.subscribe?.((message: any) => {
    switch (message.operation) {
      case "save":
        LocalStorage.set(message.details.key, message.details.value);
        if (app.ports?.localStorageUpdated) {
          app.ports.localStorageUpdated.send(message.details);
        }
        break;

      case "clear":
        LocalStorage.clear(message.details.key);
        break;

      default:
        break;
    }
  });
}
