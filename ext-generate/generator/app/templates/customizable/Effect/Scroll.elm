port module Effect.Scroll exposing
    ( toTop, toBottom, to
    , resetWindow
    )

{-|

@docs toTop, toBottom, to

@docs resetWindow

-}

import Browser.Dom
import Effect exposing (Effect)
import Json.Encode as Json


{-| -}
toTop : { id : String, onScrollFinish : msg } -> Effect msg
toTop =
    Effect.ScrollToTopOf


{-| -}
toBottom : { id : String, onScrollFinish : msg } -> Effect msg
toBottom =
    Effect.ScrollToBottomOf


{-| -}
to :
    { scrollTo : String
    , viewport : String
    , offsetY : Float
    , onScrollFinish : Result Browser.Dom.Error () -> msg
    }
    -> Effect msg
to =
    Effect.ScrollTo


port resetWindowScroll : Json.Value -> Cmd msg


{-| -}
resetWindow : Effect msg
resetWindow =
    Effect.SendToWorld
        { toPort = resetWindowScroll
        , portName = "resetWindowScroll"
        , payload =
            Json.bool True
        }
