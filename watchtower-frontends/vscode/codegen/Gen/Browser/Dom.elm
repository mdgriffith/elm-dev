module Gen.Browser.Dom exposing (annotation_, blur, call_, caseOf_, focus, getElement, getViewport, getViewportOf, make_, moduleName_, setViewport, setViewportOf, values_)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, getElement, setViewportOf, setViewport, getViewportOf, getViewport, blur, focus, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Browser", "Dom" ]


{-| Find a DOM node by `id` and focus on it. So if you wanted to focus a node
like `<input type="text" id="search-box">` you could say:

    import Browser.Dom as Dom
    import Task

    type Msg
        = NoOp

    focusSearchBox : Cmd Msg
    focusSearchBox =
        Task.attempt (\_ -> NoOp) (Dom.focus "search-box")

Notice that this code ignores the possibility that `search-box` is not used
as an `id` by any node, failing silently in that case. It would be better to
log the failure with whatever error reporting system you use.

focus: String -> Task.Task Browser.Dom.Error ()
-}
focus : String -> Elm.Expression
focus focusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "focus"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.namedWith [ "Browser", "Dom" ] "Error" []
                            , Type.unit
                            ]
                        )
                    )
            }
        )
        [ Elm.string focusArg ]


{-| Find a DOM node by `id` and make it lose focus. So if you wanted a node
like `<input type="text" id="search-box">` to lose focus you could say:

    import Browser.Dom as Dom
    import Task

    type Msg
        = NoOp

    unfocusSearchBox : Cmd Msg
    unfocusSearchBox =
        Task.attempt (\_ -> NoOp) (Dom.blur "search-box")

Notice that this code ignores the possibility that `search-box` is not used
as an `id` by any node, failing silently in that case. It would be better to
log the failure with whatever error reporting system you use.

blur: String -> Task.Task Browser.Dom.Error ()
-}
blur : String -> Elm.Expression
blur blurArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "blur"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.namedWith [ "Browser", "Dom" ] "Error" []
                            , Type.unit
                            ]
                        )
                    )
            }
        )
        [ Elm.string blurArg ]


{-| Get information on the current viewport of the browser.

![getViewport](https://elm.github.io/browser/v1/getViewport.svg)

If you want to move the viewport around (i.e. change the scroll position) you
can use [`setViewport`](#setViewport) which change the `x` and `y` of the
viewport.

getViewport: Task.Task x Browser.Dom.Viewport
-}
getViewport : Elm.Expression
getViewport =
    Elm.value
        { importFrom = [ "Browser", "Dom" ]
        , name = "getViewport"
        , annotation =
            Just
                (Type.namedWith
                    [ "Task" ]
                    "Task"
                    [ Type.var "x"
                    , Type.namedWith [ "Browser", "Dom" ] "Viewport" []
                    ]
                )
        }


{-| Just like `getViewport`, but for any scrollable DOM node. Say we have an
application with a chat box in the bottow right corner like this:

![chat](https://elm.github.io/browser/v1/chat.svg)

There are probably a whole bunch of messages that are not being shown. You
could scroll up to see them all. Well, we can think of that chat box is a
viewport into a scene!

![getViewportOf](https://elm.github.io/browser/v1/getViewportOf.svg)

This can be useful with [`setViewportOf`](#setViewportOf) to make sure new
messages always appear on the bottom.

The viewport size _does not_ include the border or margins.

**Note:** This data is collected from specific fields in JavaScript, so it
may be helpful to know that:

  - `scene.width` = [`scrollWidth`][sw]
  - `scene.height` = [`scrollHeight`][sh]
  - `viewport.x` = [`scrollLeft`][sl]
  - `viewport.y` = [`scrollTop`][st]
  - `viewport.width` = [`clientWidth`][cw]
  - `viewport.height` = [`clientHeight`][ch]

Neither [`offsetWidth`][ow] nor [`offsetHeight`][oh] are available. The theory
is that (1) the information can always be obtained by using `getElement` on a
node without margins, (2) no cases came to mind where you actually care in the
first place, and (3) it is available through ports if it is really needed.
If you have a case that really needs it though, please share your specific
scenario in an issue! Nicely presented case studies are the raw ingredients for
API improvements!

[sw]: https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollWidth
[sh]: https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollHeight
[st]: https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollTop
[sl]: https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollLeft
[cw]: https://developer.mozilla.org/en-US/docs/Web/API/Element/clientWidth
[ch]: https://developer.mozilla.org/en-US/docs/Web/API/Element/clientHeight
[ow]: https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/offsetWidth
[oh]: https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/offsetHeight

getViewportOf: String -> Task.Task Browser.Dom.Error Browser.Dom.Viewport
-}
getViewportOf : String -> Elm.Expression
getViewportOf getViewportOfArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "getViewportOf"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.namedWith [ "Browser", "Dom" ] "Error" []
                            , Type.namedWith [ "Browser", "Dom" ] "Viewport" []
                            ]
                        )
                    )
            }
        )
        [ Elm.string getViewportOfArg ]


{-| Change the `x` and `y` offset of the browser viewport immediately. For
example, you could make a command to jump to the top of the page:

    import Browser.Dom as Dom
    import Task

    type Msg
        = NoOp

    resetViewport : Cmd Msg
    resetViewport =
        Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)

This sets the viewport offset to zero.

This could be useful with `Browser.application` where you may want to reset
the viewport when the URL changes. Maybe you go to a &ldquo;new page&rdquo;
and want people to start at the top!

setViewport: Float -> Float -> Task.Task x ()
-}
setViewport : Float -> Float -> Elm.Expression
setViewport setViewportArg setViewportArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "setViewport"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.var "x", Type.unit ]
                        )
                    )
            }
        )
        [ Elm.float setViewportArg, Elm.float setViewportArg0 ]


{-| Change the `x` and `y` offset of a DOM node&rsquo;s viewport by ID. This
is common in text messaging and chat rooms, where once the messages fill the
screen, you want to always be at the very bottom of the message chain. This
way the latest message is always on screen! You could do this:

    import Browser.Dom as Dom
    import Task

    type Msg
        = NoOp

    jumpToBottom : String -> Cmd Msg
    jumpToBottom id =
        Dom.getViewportOf id
            |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
            |> Task.attempt (\_ -> NoOp)

So you could call `jumpToBottom "chat-box"` whenever you add a new message.

**Note 1:** What happens if the viewport is placed out of bounds? Where there
is no `scene` to show? To avoid this question, the `x` and `y` offsets are
clamped such that the viewport is always fully within the `scene`. So when
`jumpToBottom` sets the `y` offset of the viewport to the `height` of the
`scene` (i.e. too far!) it relies on this clamping behavior to put the viewport
back in bounds.

**Note 2:** The example ignores when the element ID is not found, but it would
be great to log that information. It means there may be a bug or a dead link
somewhere!

setViewportOf: String -> Float -> Float -> Task.Task Browser.Dom.Error ()
-}
setViewportOf : String -> Float -> Float -> Elm.Expression
setViewportOf setViewportOfArg setViewportOfArg0 setViewportOfArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "setViewportOf"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.float, Type.float ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.namedWith [ "Browser", "Dom" ] "Error" []
                            , Type.unit
                            ]
                        )
                    )
            }
        )
        [ Elm.string setViewportOfArg
        , Elm.float setViewportOfArg0
        , Elm.float setViewportOfArg1
        ]


{-| Get position information about specific elements. Say we put
`id "jesting-aside"` on the seventh paragraph of the text. When we call
`getElement "jesting-aside"` we would get the following information:

![getElement](https://elm.github.io/browser/v1/getElement.svg)

This can be useful for:

  - **Scrolling** &mdash; Pair this information with `setViewport` to scroll
    specific elements into view. This gives you a lot of control over where exactly
    the element would be after the viewport moved.

  - **Drag and Drop** &mdash; As of this writing, `touchmove` events do not tell
    you which element you are currently above. To figure out if you have dragged
    something over the target, you could see if the `pageX` and `pageY` of the
    touch are inside the `x`, `y`, `width`, and `height` of the target element.

**Note:** This corresponds to JavaScript&rsquo;s [`getBoundingClientRect`][gbcr],
so **the element&rsquo;s margins are included in its `width` and `height`**.
With scrolling, maybe you want to include the margins. With drag-and-drop, you
probably do not, so some folks set the margins to zero and put the target
element in a `<div>` that adds the spacing. Just something to be aware of!

[gbcr]: https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect

getElement: String -> Task.Task Browser.Dom.Error Browser.Dom.Element
-}
getElement : String -> Elm.Expression
getElement getElementArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "getElement"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.namedWith [ "Browser", "Dom" ] "Error" []
                            , Type.namedWith [ "Browser", "Dom" ] "Element" []
                            ]
                        )
                    )
            }
        )
        [ Elm.string getElementArg ]


annotation_ :
    { error : Type.Annotation
    , viewport : Type.Annotation
    , element : Type.Annotation
    }
annotation_ =
    { error = Type.namedWith [ "Browser", "Dom" ] "Error" []
    , viewport =
        Type.alias
            moduleName_
            "Viewport"
            []
            (Type.record
                [ ( "scene"
                  , Type.record
                        [ ( "width", Type.float ), ( "height", Type.float ) ]
                  )
                , ( "viewport"
                  , Type.record
                        [ ( "x", Type.float )
                        , ( "y", Type.float )
                        , ( "width", Type.float )
                        , ( "height", Type.float )
                        ]
                  )
                ]
            )
    , element =
        Type.alias
            moduleName_
            "Element"
            []
            (Type.record
                [ ( "scene"
                  , Type.record
                        [ ( "width", Type.float ), ( "height", Type.float ) ]
                  )
                , ( "viewport"
                  , Type.record
                        [ ( "x", Type.float )
                        , ( "y", Type.float )
                        , ( "width", Type.float )
                        , ( "height", Type.float )
                        ]
                  )
                , ( "element"
                  , Type.record
                        [ ( "x", Type.float )
                        , ( "y", Type.float )
                        , ( "width", Type.float )
                        , ( "height", Type.float )
                        ]
                  )
                ]
            )
    }


make_ :
    { notFound : Elm.Expression -> Elm.Expression
    , viewport :
        { scene : Elm.Expression, viewport : Elm.Expression } -> Elm.Expression
    , element :
        { scene : Elm.Expression
        , viewport : Elm.Expression
        , element : Elm.Expression
        }
        -> Elm.Expression
    }
make_ =
    { notFound =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Dom" ]
                    , name = "NotFound"
                    , annotation = Just (Type.namedWith [] "Error" [])
                    }
                )
                [ ar0 ]
    , viewport =
        \viewport_args ->
            Elm.withType
                (Type.alias
                    [ "Browser", "Dom" ]
                    "Viewport"
                    []
                    (Type.record
                        [ ( "scene"
                          , Type.record
                                [ ( "width", Type.float )
                                , ( "height", Type.float )
                                ]
                          )
                        , ( "viewport"
                          , Type.record
                                [ ( "x", Type.float )
                                , ( "y", Type.float )
                                , ( "width", Type.float )
                                , ( "height", Type.float )
                                ]
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "scene" viewport_args.scene
                    , Tuple.pair "viewport" viewport_args.viewport
                    ]
                )
    , element =
        \element_args ->
            Elm.withType
                (Type.alias
                    [ "Browser", "Dom" ]
                    "Element"
                    []
                    (Type.record
                        [ ( "scene"
                          , Type.record
                                [ ( "width", Type.float )
                                , ( "height", Type.float )
                                ]
                          )
                        , ( "viewport"
                          , Type.record
                                [ ( "x", Type.float )
                                , ( "y", Type.float )
                                , ( "width", Type.float )
                                , ( "height", Type.float )
                                ]
                          )
                        , ( "element"
                          , Type.record
                                [ ( "x", Type.float )
                                , ( "y", Type.float )
                                , ( "width", Type.float )
                                , ( "height", Type.float )
                                ]
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "scene" element_args.scene
                    , Tuple.pair "viewport" element_args.viewport
                    , Tuple.pair "element" element_args.element
                    ]
                )
    }


caseOf_ :
    { error :
        Elm.Expression
        -> { errorTags_0_0 | notFound : Elm.Expression -> Elm.Expression }
        -> Elm.Expression
    }
caseOf_ =
    { error =
        \errorExpression errorTags ->
            Elm.Case.custom
                errorExpression
                (Type.namedWith [ "Browser", "Dom" ] "Error" [])
                [ Elm.Case.branch1
                    "NotFound"
                    ( "string.String", Type.string )
                    errorTags.notFound
                ]
    }


call_ :
    { focus : Elm.Expression -> Elm.Expression
    , blur : Elm.Expression -> Elm.Expression
    , getViewportOf : Elm.Expression -> Elm.Expression
    , setViewport : Elm.Expression -> Elm.Expression -> Elm.Expression
    , setViewportOf :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , getElement : Elm.Expression -> Elm.Expression
    }
call_ =
    { focus =
        \focusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Dom" ]
                    , name = "focus"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Task" ]
                                    "Task"
                                    [ Type.namedWith
                                        [ "Browser", "Dom" ]
                                        "Error"
                                        []
                                    , Type.unit
                                    ]
                                )
                            )
                    }
                )
                [ focusArg ]
    , blur =
        \blurArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Dom" ]
                    , name = "blur"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Task" ]
                                    "Task"
                                    [ Type.namedWith
                                        [ "Browser", "Dom" ]
                                        "Error"
                                        []
                                    , Type.unit
                                    ]
                                )
                            )
                    }
                )
                [ blurArg ]
    , getViewportOf =
        \getViewportOfArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Dom" ]
                    , name = "getViewportOf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Task" ]
                                    "Task"
                                    [ Type.namedWith
                                        [ "Browser", "Dom" ]
                                        "Error"
                                        []
                                    , Type.namedWith
                                        [ "Browser", "Dom" ]
                                        "Viewport"
                                        []
                                    ]
                                )
                            )
                    }
                )
                [ getViewportOfArg ]
    , setViewport =
        \setViewportArg setViewportArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Dom" ]
                    , name = "setViewport"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Task" ]
                                    "Task"
                                    [ Type.var "x", Type.unit ]
                                )
                            )
                    }
                )
                [ setViewportArg, setViewportArg0 ]
    , setViewportOf =
        \setViewportOfArg setViewportOfArg0 setViewportOfArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Dom" ]
                    , name = "setViewportOf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Task" ]
                                    "Task"
                                    [ Type.namedWith
                                        [ "Browser", "Dom" ]
                                        "Error"
                                        []
                                    , Type.unit
                                    ]
                                )
                            )
                    }
                )
                [ setViewportOfArg, setViewportOfArg0, setViewportOfArg1 ]
    , getElement =
        \getElementArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Dom" ]
                    , name = "getElement"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Task" ]
                                    "Task"
                                    [ Type.namedWith
                                        [ "Browser", "Dom" ]
                                        "Error"
                                        []
                                    , Type.namedWith
                                        [ "Browser", "Dom" ]
                                        "Element"
                                        []
                                    ]
                                )
                            )
                    }
                )
                [ getElementArg ]
    }


values_ :
    { focus : Elm.Expression
    , blur : Elm.Expression
    , getViewport : Elm.Expression
    , getViewportOf : Elm.Expression
    , setViewport : Elm.Expression
    , setViewportOf : Elm.Expression
    , getElement : Elm.Expression
    }
values_ =
    { focus =
        Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "focus"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.namedWith [ "Browser", "Dom" ] "Error" []
                            , Type.unit
                            ]
                        )
                    )
            }
    , blur =
        Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "blur"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.namedWith [ "Browser", "Dom" ] "Error" []
                            , Type.unit
                            ]
                        )
                    )
            }
    , getViewport =
        Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "getViewport"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Task" ]
                        "Task"
                        [ Type.var "x"
                        , Type.namedWith [ "Browser", "Dom" ] "Viewport" []
                        ]
                    )
            }
    , getViewportOf =
        Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "getViewportOf"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.namedWith [ "Browser", "Dom" ] "Error" []
                            , Type.namedWith [ "Browser", "Dom" ] "Viewport" []
                            ]
                        )
                    )
            }
    , setViewport =
        Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "setViewport"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.var "x", Type.unit ]
                        )
                    )
            }
    , setViewportOf =
        Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "setViewportOf"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.float, Type.float ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.namedWith [ "Browser", "Dom" ] "Error" []
                            , Type.unit
                            ]
                        )
                    )
            }
    , getElement =
        Elm.value
            { importFrom = [ "Browser", "Dom" ]
            , name = "getElement"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Task" ]
                            "Task"
                            [ Type.namedWith [ "Browser", "Dom" ] "Error" []
                            , Type.namedWith [ "Browser", "Dom" ] "Element" []
                            ]
                        )
                    )
            }
    }


