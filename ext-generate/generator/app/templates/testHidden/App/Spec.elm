module App.Spec exposing (onPage, start)

import App
import App.Spec.Effect
import App.Spec.Listen
import Spec
import Spec.Program


start options =
    let
        application =
            App.test options.app
    in
    Spec.Program.start
        { url = options.url, flags = options.flags, time = options.time }
        { init = application.init
        , update = application.update
        , view = application.view
        , onUrlChange = application.onUrlChange
        , onUrlRequest = application.onUrlRequest
        , effects =
            App.Spec.Effect.simulate
                { callbacks = application.callbacks
                , time = options.time
                , zone = options.zone
                , zoneName = options.zoneName
                , httpTarget = options.httpTarget
                }
        , subscriptions = application.subscriptions >> App.Spec.Listen.simulate application.callbacks
        , timers = application.subscriptions >> App.Spec.Listen.timers
        }


onPage options =
    Spec.onPage { start = \_ -> start options, init = options.init, tests = options.tests }
