module VSCode.SyntaxColors exposing
    ( declaration
    , field
    , keyword
    , moduleName
    , punctuation
    , type_
    , typevar
    , variant
    )

import Element
import Html.Attributes as Attr


{-| VSCode does not use their color theme thing for syntax highlighting.
The colors in <https://code.visualstudio.com/api/references/theme-color> don't capture actual colors used for syntax highlighting which is fairly frustrating.

After diving in, I found that they have specialized classes that are not referenced in their ColorTheme css variables.

They're using a bitfield when tokenizing elements as described here: <https://code.visualstudio.com/blogs/2017/02/08/syntax-highlighting-optimizations>
as a performance optimization. But there doesn't seem to be a way to get these values programatically?
So, here is the stylesheet that is used for my editor.

Which, I'm just going to bake in.

.mtk1 { color: #f8f8f8; }
.mtk2 { color: #000000; }
.mtk3 { color: #798188; }
.mtk4 { color: #b8d977; }
.mtk5 { color: #72aaca; }
.mtk6 { color: #fa9a4b; }
.mtk7 { color: #f6f080; }
.mtk8 { color: #c4e2f2; }
.mtk9 { color: #fb9a4b; }
.mtk10 { color: rgba(216, 41, 13, 0.75); }
.mtk11 { color: rgba(176, 179, 186, 0.08); }
.mtk12 { color: rgba(177, 179, 186, 0.13); }
.mtk13 { color: #b7d877; }
.mtk14 { color: #ffb454; }
.mtk15 { color: #edef7d; }
.mtk16 { color: #8996a8; }
.mtk17 { color: #afc4db; }
.mtk18 { color: #f5f080; }
.mtk19 { color: #b6d877; }
.mtk20 { color: #f6aa11; }
.mtk21 { color: #edf080; }
.mtk22 { color: #eb939a; }
.mtk23 { color: #0e2231; }
.mtk24 { color: #d03620; }
.mtk25 { color: #c4b14a; }
.mtk26 { color: #41a83e; }
.mtk27 { color: rgba(255, 255, 170, 0.31); }
.mtk28 { color: rgba(255, 74, 82, 0.31); }
.mtk29 { color: rgba(255, 0, 0, 0.31); }
.mtk30 { color: rgba(223, 148, 0, 0.31); }
.mtk31 { color: rgba(255, 255, 255, 0.2); }
.mtk32 { color: #65a4a4; }
.mtk33 { color: #f7f09d; }
.mtk34 { color: #ff3a83; }
.mtk35 { color: #f1e94b; }
.mtk36 { color: #ffffff; }
.mtk37 { color: #ffaa00; }
.mtk38 { color: #73817d; }
.mtk39 { color: #452323; }
.mtk40 { color: #234523; }
.mtk41 { color: #232345; }
.mtk42 { color: #454523; }
.mtk43 { color: #452345; }
.mtk44 { color: #234545; }
.mtk45 { color: #634141; }
.mtk46 { color: #416341; }
.mtk47 { color: #414163; }
.mtk48 { color: #636341; }
.mtk49 { color: #634163; }
.mtk50 { color: #416363; }
.mtk51 { color: #6796e6; }
.mtk52 { color: #cd9731; }
.mtk53 { color: #f44747; }
.mtk54 { color: #b267e6; }
.mtki { font-style: italic; }
.mtkb { font-weight: bold; }
.mtku { text-decoration: underline; text-underline-position: under; }
.mtks { text-decoration: line-through; }
.mtks.mtku { text-decoration: underline line-through; text-underline-position: under; }

-}
keyword : Element.Attribute msg
keyword =
    Element.htmlAttribute (Attr.style "color" "#fa9a4b")


typevar : Element.Attribute msg
typevar =
    Element.htmlAttribute (Attr.style "color" "#fa9a4b")


moduleName : Element.Attribute msg
moduleName =
    Element.htmlAttribute (Attr.style "color" "#72aaca")


declaration : Element.Attribute msg
declaration =
    Element.htmlAttribute (Attr.style "color" "#72aaca")


field : Element.Attribute msg
field =
    Element.htmlAttribute (Attr.style "color" "#72aaca")


type_ : Element.Attribute msg
type_ =
    Element.htmlAttribute (Attr.style "color" "#f6f080")


punctuation : Element.Attribute msg
punctuation =
    Element.htmlAttribute (Attr.style "color" "#798188")


variant : Element.Attribute msg
variant =
    Element.htmlAttribute (Attr.style "color" "#b8d977")
