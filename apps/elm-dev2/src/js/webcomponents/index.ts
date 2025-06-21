// import TipTap from "./js/tiptap";
import ElmPortal from "./elm-portal";
import ElmDev from "./interactive";
import ElmEmbedded from "./elm-embedded";

export default function include() {
  ElmPortal({ mountId: "elm-portal-target" });
  ElmDev();
  ElmEmbedded();
  // TipTap();
}
