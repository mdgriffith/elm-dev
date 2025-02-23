module Main exposing (main)

{-| -}

import App
import App.Page.Id
import App.Resources
import App.View.Id
import Browser
import Effect exposing (Effect)
import Effect.Nav
import Effect.Page
import Effect.Scroll
import Html
import Html.Attributes as Attr
import Listen
import Theme
import Theme.Color
import Ui.Nav
import Url


type alias Model =
    {}


{-| -}
main : App.App Model Msg
main =
    App.app
        { init =
            \resources flags url ->
                ( {}
                , Effect.batch
                    [ Effect.Nav.toUrl url
                    , Effect.Page.loadAt App.View.Id.Detail (App.Page.Id.Reference {})
                    ]
                )
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        , update = update
        , subscriptions =
            \resources model ->
                Listen.none
        , toCmd = toCmd
        , toSub = toSub
        , view =
            \resources toAppMsg model regions ->
                case regions.primary of
                    Nothing ->
                        { title = "Nothing"
                        , body = [ Html.text "" ]
                        }

                    Just (App.Loading _) ->
                        { title = "Loading"
                        , body = [ Html.text "Loading" ]
                        }

                    Just App.NotFound ->
                        --
                        { title = "Not found"
                        , body = [ Html.text "Not found" ]
                        }

                    Just (App.Error error) ->
                        -- error is a type you control that lives at App.Page.Error
                        { title = "Not found"
                        , body = [ Html.text "Not found" ]
                        }

                    Just (App.View page) ->
                        view resources toAppMsg model regions page
        }


toSub : App.Resources.Resources -> App.SubOptions Msg -> Model -> Listen.Listen (App.Msg Msg) -> Sub (App.Msg Msg)
toSub resources options model sub =
    Listen.toSubscription options sub


toCmd : App.Resources.Resources -> App.CmdOptions Msg -> Model -> Effect.Effect (App.Msg Msg) -> Cmd (App.Msg Msg)
toCmd resources options model effect =
    Effect.toCmd options
        (\urlBase ->
            case urlBase of
                Effect.UrlApi ->
                    { headers = []
                    , urlBase = ""
                    }

                Effect.UrlStaticFile ->
                    { headers = []
                    , urlBase = ""
                    }

                Effect.UrlCustom base ->
                    { headers = []
                    , urlBase = base
                    }
        )
        effect


heightWindow =
    Attr.style "height" "100vh"


view resources toAppMsg model regions innerView =
    { title = innerView.title
    , body =
        [ stylesheet
        , Html.div
            [ Theme.Color.backgroundDefault
            , Theme.Color.textDefault
            , Theme.setMode Theme.Default
            ]
            [ Theme.row.zero []
                [ Ui.Nav.view {}
                , innerView.body
                , viewDetails regions.detail
                ]
            ]
        ]
    }


viewDetails details =
    case details of
        Nothing ->
            Html.text ""

        Just (App.Loading _) ->
            Html.text ""

        Just App.NotFound ->
            Html.text ""

        Just (App.Error error) ->
            Html.text ""

        Just (App.View page) ->
            Html.div
                [ Attr.style "position" "fixed"
                , Attr.style "top" "0"
                , Attr.style "bottom" "0"
                , Attr.style "right" "0"
                , Attr.style "width" "500px"
                , Theme.pad.sm
                , Theme.Color.backgroundDefault
                , Attr.style "overflow" "auto"
                ]
                [ page.body ]


stylesheet =
    Html.node "style"
        []
        [ Html.text """
html, head, body {
  background-color: white;
  margin: 0;
}

@media (prefers-color-scheme: dark) {
  html {
    background-color: var(--grey5);
  }
}

body {
  font-family: 'Source Sans Pro', 'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif;
  color: #000E16;
  display: flex;
  min-height: 100vh;
  flex-direction: column;
}

h1, h2, h3, h4 {
  font-weight: normal;
  margin:0;
}

p, li {
  line-height: 1.5em;
}

a { color: #1293D8; text-decoration: none; }
a:hover { text-decoration: underline; }
a .light { color: #5FABDC; }

pre {
  margin: 0;
  padding: 10px;
  background-color: rgb(254,254,254);
  border-style: solid;
  border-width: 1px;
  border-color: rgb(245,245,245);
  border-radius: 6px;
  overflow-x: auto;
}



/* HOME */

.home-summaries ul {
  list-style-type: none;
  padding-left: 1em;
}



/* CENTER */

.center {
  width: 920px;
  margin-left: auto;
  margin-right: auto;
}



/* HEADER */

.header {
  background-color: #5FABDC;
  width: calc(100% - 40px);
  padding-left: 20px;
  padding-right: 20px;
  overflow-x: hidden;
}

.nav {
  max-width: 920px;
  height: 64px;
  margin: 0 auto;
  display: flex;
  align-items: center;
}

.nav { color: white; }
.nav a { color: white; }
.nav h1 { font-size: 24px; }
.spacey-char { margin: 0 10px; }

.header-underbar {
  background-color: #f2e19e;
  margin: 0;
}

.version-warning {
  margin: 0 0 0 calc((100% - 920px) / 2);
  padding: 7px 0;
  color: #750707;
}

.version-warning a {
  color: #750707;
  text-decoration: underline;
}



/* FOOTER */


.footer {
  text-align: center;
  margin-top: 4em;
  border-top: 1px solid #eeeeee;
  padding: 2em 0;
  color: #bbbbbb;
}


.grey-link {
  color: #bbbbbb;
  text-decoration: underline;
  cursor: pointer;
}

.grey-link:hover {
  color: #bbbbbb;
}


/* DOCUMENTATION */


.block-list {
  width: 600px;
  display: inline-block;
  vertical-align: top;
}

.block-list-title {
  font-size: 3em;
  margin-bottom: 16px;
}

.markdown-block h1 {
  margin-top: 2em;
  margin-bottom: 0.5em;
}

.docs-block {
  border-top: 1px solid #eeeeee;
  margin-top: 1em;
  margin-bottom: 2em;
}


.docs-header {
  white-space: pre;
  font-family: 'Source Code Pro', Consolas, "Liberation Mono", Menlo, Courier, monospace;
  padding-top: 10px;
  padding-bottom: 10px;
}


.docs-comment {
  overflow: hidden;
  padding-left: 36px;
}

.docs-comment img {
  max-width: 500px;
}



/* ABOUT */


.pkg-about table {
  font-family: 'Source Code Pro', Consolas, "Liberation Mono", Menlo, Courier, monospace;
  width: 100%;
}

.pkg-about td {
  padding: 4px 0;
}



/* PACKAGE NAVIGATION */


.pkg-nav {
  width: 200px;
  display: inline-block;
  vertical-align: top;
  padding: 20px;
  margin: 20px 20px 20px 40px;
  border-left: 1px solid #eeeeee;
}

.pkg-nav h2 {
  margin-bottom: 0;
}

.pkg-nav input {
  width: 200px;
  font-size: 1em;
  padding: 4px;
  margin: 10px 0;
  border: 1px solid #eeeeee;
  border-radius: 6px;
}

.pkg-nav ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
}

.pkg-nav ul ul {
  list-style-type: none;
  margin: 0;
  padding-left: 20px;
}

.pkg-nav-module {
  font-family: 'Source Code Pro', Consolas, "Liberation Mono", Menlo, Courier, monospace;
  text-overflow: ellipsis;
}

.pkg-nav-value {
  font-family: 'Source Code Pro', Consolas, "Liberation Mono", Menlo, Courier, monospace;
  text-overflow: ellipsis;
}

.pkg-nav-search-chunk {
  padding-bottom: 10px;
}



/* CATALOG */


.catalog {
  width: 600px;
  display: inline-block;
  vertical-align: top;
}


.catalog input {
  width: 578px;
  font-size: 1.5em;
  padding: 10px;
  outline: none;
  border: 1px solid #eeeeee;
  border-radius: 8px;
  margin-top: 40px;
  margin-bottom: 10px;
}


.pkg-hint {
  padding: 20px;
  background-color: #fcfcfc;
  border-radius: 8px;
}

.pkg-summary {
  padding: 20px 0;
  border-bottom: 1px solid #eeeeee;
}

.pkg-summary h1 {
  margin: 0;
  font-size: 1.5em;
  display: inline-block;
}

.pkg-summary-desc {
  margin: 0.5em 0;
}

.pkg-summary-hints {
  float: right;
  font-size: 1em;
  padding-top: 0.5em;
  color: #bbbbbb;
}

.pkg-summary-hints a {
  color: #bbbbbb;
}



/* CATALOG SIDEBAR */


.catalog-sidebar {
  width: 200px;
  display: inline-block;
  vertical-align: top;
  padding: 20px;
  margin: 20px 20px 20px 40px;
  border-left: 1px solid #eeeeee;
}

.catalog-sidebar h2 {
  font-size: 1.5em;
  padding: 10px;
  margin: 0;
}

.catalog-sidebar ul {
  list-style-type: none;
  margin: 0;
  padding-left: 20px;
  padding-bottom: 10px;
}



/* CODE */


code {
  font-family: 'Source Code Pro', Consolas, "Liberation Mono", Menlo, Courier, monospace;
}

/* I heard using :not() is slow for reflows.
Not really any of those on the website though AFAIK.
*/
:not(pre) > code {
  padding: 0;
  padding-top: 0.2em;
  padding-bottom: 0.2em;
  margin: 0;
  font-size: 85%;
  background-color: rgba(0,0,0,0.04);
  border-radius: 3px;
}

:not(pre) > code::before, :not(pre) > code::after {
  letter-spacing: -0.2em;
  content: "\\00a0";
}

.column {
  display: flex;
  flex-direction: column;
  box-sizing: border-box;
}

.row {
  display: flex;
  flex-direction: row;
  box-sizing: border-box;
}


.ellipsis {
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}



"""
        ]


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest


update : App.Resources.Resources -> Msg -> Model -> ( Model, Effect Msg )
update resources msg model =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( model, Effect.Nav.pushUrl (Url.toString url) )

        UrlRequested (Browser.External urlStr) ->
            ( model, Effect.Nav.load urlStr )

        UrlChanged url ->
            ( model
            , Effect.batch
                [ Effect.Nav.toUrl url
                , Effect.Scroll.resetWindow
                ]
            )
