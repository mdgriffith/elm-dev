// import TipTap from "./js/tiptap";
import ElmPortal from "./elm-portal";
import ElmDev from "./interactive";

export default function include() {
  ElmPortal({ mountId: "elm-portal-target" });
  ElmDev();
  // TipTap();
}
