// @ts-ignore
import { Elm } from "./app/Main.elm";
import * as LocalStorage from "./js/local-storage";
import * as Ports from "./js/ports";
import Webcomponents from "./js/webcomponents";

// Import all generated CSS files
import.meta.glob("../.elm-prefab/**/*.css", { eager: true });

// Include any custom elements we need.
Webcomponents();

// Boot up the Elm App
const app = Elm.Main.init({
  flags: { now: Date.now(), localStorage: LocalStorage.getAll() },
});

// Connect ports
Ports.connect(app);
