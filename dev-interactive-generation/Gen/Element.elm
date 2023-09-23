module Gen.Element exposing (above, alignBottom, alignLeft, alignRight, alignTop, alpha, annotation_, behindContent, below, call_, caseOf_, centerX, centerY, classifyDevice, clip, clipX, clipY, column, download, downloadAs, el, explain, fill, fillPortion, focusStyle, focused, forceHover, fromRgb, fromRgb255, height, html, htmlAttribute, image, inFront, indexedTable, layout, layoutWith, link, make_, map, mapAttribute, maximum, minimum, modular, moduleName_, mouseDown, mouseOver, moveDown, moveLeft, moveRight, moveUp, newTabLink, noHover, noStaticStyleSheet, none, onLeft, onRight, padding, paddingEach, paddingXY, paragraph, pointer, px, rgb, rgb255, rgba, rgba255, rotate, row, scale, scrollbarX, scrollbarY, scrollbars, shrink, spaceEvenly, spacing, spacingXY, table, text, textColumn, toRgb, transparent, values_, width, wrappedRow)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, htmlAttribute, html, mapAttribute, map, modular, classifyDevice, focused, mouseDown, mouseOver, behindContent, inFront, onLeft, onRight, below, above, toRgb, fromRgb255, fromRgb, rgba255, rgb255, rgb, rgba, image, downloadAs, download, newTabLink, link, focusStyle, noHover, forceHover, noStaticStyleSheet, layoutWith, layout, scrollbarY, scrollbarX, scrollbars, clipY, clipX, clip, scale, rotate, moveLeft, moveRight, moveDown, moveUp, pointer, alpha, transparent, alignBottom, alignTop, alignRight, alignLeft, centerY, centerX, spaceEvenly, spacingXY, spacing, paddingEach, paddingXY, padding, explain, minimum, maximum, fillPortion, fill, shrink, px, height, width, indexedTable, table, textColumn, paragraph, column, wrappedRow, row, el, text, none, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Element" ]


{-| When you want to render exactly nothing.

none: Element.Element msg
-}
none : Elm.Expression
none =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "none"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Element" [ Type.var "msg" ])
        }


{-| Create some plain text.

    text "Hello, you stylish developer!"

**Note** text does not wrap by default. In order to get text to wrap, check out `paragraph`!

text: String -> Element.Element msg
-}
text : String -> Elm.Expression
text textArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string textArg ]


{-| The basic building block of your layout.

You can think of an `el` as a `div`, but it can only have one child.

If you want multiple children, you'll need to use something like `row` or `column`

    import Element exposing (Element, rgb)
    import Element.Background as Background
    import Element.Border as Border

    myElement : Element msg
    myElement =
        Element.el
            [ Background.color (rgb 0 0.5 0)
            , Border.color (rgb 0 0.7 0)
            ]
            (Element.text "You've made a stylish element!")

el: List (Element.Attribute msg) -> Element.Element msg -> Element.Element msg
-}
el : List Elm.Expression -> Elm.Expression -> Elm.Expression
el elArg elArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "el"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list elArg, elArg0 ]


{-| row: 
    List (Element.Attribute msg)
    -> List (Element.Element msg)
    -> Element.Element msg
-}
row : List Elm.Expression -> List Elm.Expression -> Elm.Expression
row rowArg rowArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "row"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list rowArg, Elm.list rowArg0 ]


{-| Same as `row`, but will wrap if it takes up too much horizontal space.

wrappedRow: 
    List (Element.Attribute msg)
    -> List (Element.Element msg)
    -> Element.Element msg
-}
wrappedRow : List Elm.Expression -> List Elm.Expression -> Elm.Expression
wrappedRow wrappedRowArg wrappedRowArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "wrappedRow"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list wrappedRowArg, Elm.list wrappedRowArg0 ]


{-| column: 
    List (Element.Attribute msg)
    -> List (Element.Element msg)
    -> Element.Element msg
-}
column : List Elm.Expression -> List Elm.Expression -> Elm.Expression
column columnArg columnArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "column"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list columnArg, Elm.list columnArg0 ]


{-| A paragraph will layout all children as wrapped, inline elements.

    import Element exposing (el, paragraph, text)
    import Element.Font as Font

    view =
        paragraph []
            [ text "lots of text ...."
            , el [ Font.bold ] (text "this is bold")
            , text "lots of text ...."
            ]

This is really useful when you want to markup text by having some parts be bold, or some be links, or whatever you so desire.

Also, if a child element has `alignLeft` or `alignRight`, then it will be moved to that side and the text will flow around it, (ah yes, `float` behavior).

This makes it particularly easy to do something like a [dropped capital](https://en.wikipedia.org/wiki/Initial).

    import Element exposing (alignLeft, el, padding, paragraph, text)
    import Element.Font as Font

    view =
        paragraph []
            [ el
                [ alignLeft
                , padding 5
                ]
                (text "S")
            , text "o much text ...."
            ]

Which will look something like

![A paragraph where the first letter is twice the height of the others](https://mdgriffith.gitbooks.io/style-elements/content/assets/Screen%20Shot%202017-08-25%20at%209.41.52%20PM.png)

**Note** `spacing` on a paragraph will set the pixel spacing between lines.

paragraph: 
    List (Element.Attribute msg)
    -> List (Element.Element msg)
    -> Element.Element msg
-}
paragraph : List Elm.Expression -> List Elm.Expression -> Elm.Expression
paragraph paragraphArg paragraphArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "paragraph"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list paragraphArg, Elm.list paragraphArg0 ]


{-| Now that we have a paragraph, we need some way to attach a bunch of paragraph's together.

To do that we can use a `textColumn`.

The main difference between a `column` and a `textColumn` is that `textColumn` will flow the text around elements that have `alignRight` or `alignLeft`, just like we just saw with paragraph.

In the following example, we have a `textColumn` where one child has `alignLeft`.

    Element.textColumn [ spacing 10, padding 10 ]
        [ paragraph [] [ text "lots of text ...." ]
        , el [ alignLeft ] none
        , paragraph [] [ text "lots of text ...." ]
        ]

Which will result in something like:

![A text layout where an image is on the left.](https://mdgriffith.gitbooks.io/style-elements/content/assets/Screen%20Shot%202017-08-25%20at%208.42.39%20PM.png)

textColumn: 
    List (Element.Attribute msg)
    -> List (Element.Element msg)
    -> Element.Element msg
-}
textColumn : List Elm.Expression -> List Elm.Expression -> Elm.Expression
textColumn textColumnArg textColumnArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "textColumn"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list textColumnArg, Elm.list textColumnArg0 ]


{-| Show some tabular data.

Start with a list of records and specify how each column should be rendered.

So, if we have a list of `persons`:

    type alias Person =
        { firstName : String
        , lastName : String
        }

    persons : List Person
    persons =
        [ { firstName = "David"
          , lastName = "Bowie"
          }
        , { firstName = "Florence"
          , lastName = "Welch"
          }
        ]

We could render it using

    Element.table []
        { data = persons
        , columns =
            [ { header = Element.text "First Name"
              , width = fill
              , view =
                    \person ->
                        Element.text person.firstName
              }
            , { header = Element.text "Last Name"
              , width = fill
              , view =
                    \person ->
                        Element.text person.lastName
              }
            ]
        }

**Note:** Sometimes you might not have a list of records directly in your model. In this case it can be really nice to write a function that transforms some part of your model into a list of records before feeding it into `Element.table`.

table: 
    List (Element.Attribute msg)
    -> { data : List records, columns : List (Element.Column records msg) }
    -> Element.Element msg
-}
table :
    List Elm.Expression
    -> { data : List Elm.Expression, columns : List Elm.Expression }
    -> Elm.Expression
table tableArg tableArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "table"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "data", Type.list (Type.var "records") )
                            , ( "columns"
                              , Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Column"
                                        [ Type.var "records", Type.var "msg" ]
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list tableArg
        , Elm.record
            [ Tuple.pair "data" (Elm.list tableArg0.data)
            , Tuple.pair "columns" (Elm.list tableArg0.columns)
            ]
        ]


{-| Same as `Element.table` except the `view` for each column will also receive the row index as well as the record.

indexedTable: 
    List (Element.Attribute msg)
    -> { data : List records, columns : List (Element.IndexedColumn records msg) }
    -> Element.Element msg
-}
indexedTable :
    List Elm.Expression
    -> { data : List Elm.Expression, columns : List Elm.Expression }
    -> Elm.Expression
indexedTable indexedTableArg indexedTableArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "indexedTable"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "data", Type.list (Type.var "records") )
                            , ( "columns"
                              , Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "IndexedColumn"
                                        [ Type.var "records", Type.var "msg" ]
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list indexedTableArg
        , Elm.record
            [ Tuple.pair "data" (Elm.list indexedTableArg0.data)
            , Tuple.pair "columns" (Elm.list indexedTableArg0.columns)
            ]
        ]


{-| width: Element.Length -> Element.Attribute msg -}
width : Elm.Expression -> Elm.Expression
width widthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "width"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Length" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ widthArg ]


{-| height: Element.Length -> Element.Attribute msg -}
height : Elm.Expression -> Elm.Expression
height heightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "height"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Length" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ heightArg ]


{-| px: Int -> Element.Length -}
px : Int -> Elm.Expression
px pxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "px"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Element" ] "Length" [])
                    )
            }
        )
        [ Elm.int pxArg ]


{-| Shrink an element to fit its contents.

shrink: Element.Length
-}
shrink : Elm.Expression
shrink =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "shrink"
        , annotation = Just (Type.namedWith [ "Element" ] "Length" [])
        }


{-| Fill the available space. The available space will be split evenly between elements that have `width fill`.

fill: Element.Length
-}
fill : Elm.Expression
fill =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "fill"
        , annotation = Just (Type.namedWith [ "Element" ] "Length" [])
        }


{-| Sometimes you may not want to split available space evenly. In this case you can use `fillPortion` to define which elements should have what portion of the available space.

So, two elements, one with `width (fillPortion 2)` and one with `width (fillPortion 3)`. The first would get 2 portions of the available space, while the second would get 3.

**Also:** `fill == fillPortion 1`

fillPortion: Int -> Element.Length
-}
fillPortion : Int -> Elm.Expression
fillPortion fillPortionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "fillPortion"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Element" ] "Length" [])
                    )
            }
        )
        [ Elm.int fillPortionArg ]


{-| Add a maximum to a length.

    el
        [ height
            (fill
                |> maximum 300
            )
        ]
        (text "I will stop at 300px")

maximum: Int -> Element.Length -> Element.Length
-}
maximum : Int -> Elm.Expression -> Elm.Expression
maximum maximumArg maximumArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "maximum"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.namedWith [ "Element" ] "Length" [] ]
                        (Type.namedWith [ "Element" ] "Length" [])
                    )
            }
        )
        [ Elm.int maximumArg, maximumArg0 ]


{-| Similarly you can set a minimum boundary.

     el
        [ height
            (fill
                |> maximum 300
                |> minimum 30
            )

        ]
        (text "I will stop at 300px")

minimum: Int -> Element.Length -> Element.Length
-}
minimum : Int -> Elm.Expression -> Elm.Expression
minimum minimumArg minimumArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "minimum"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.namedWith [ "Element" ] "Length" [] ]
                        (Type.namedWith [ "Element" ] "Length" [])
                    )
            }
        )
        [ Elm.int minimumArg, minimumArg0 ]


{-| Highlight the borders of an element and it's children below. This can really help if you're running into some issue with your layout!

**Note** This attribute needs to be handed `Debug.todo` in order to work, even though it won't do anything with it. This is a safety measure so you don't accidently ship code with `explain` in it, as Elm won't compile with `--optimize` if you still have a `Debug` statement in your code.

    el
        [ Element.explain Debug.todo
        ]
        (text "Help, I'm being debugged!")

explain: Element.Todo -> Element.Attribute msg
-}
explain : Elm.Expression -> Elm.Expression
explain explainArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "explain"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Todo" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ explainArg ]


{-| padding: Int -> Element.Attribute msg -}
padding : Int -> Elm.Expression
padding paddingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "padding"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int paddingArg ]


{-| Set horizontal and vertical padding.

paddingXY: Int -> Int -> Element.Attribute msg
-}
paddingXY : Int -> Int -> Elm.Expression
paddingXY paddingXYArg paddingXYArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "paddingXY"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int paddingXYArg, Elm.int paddingXYArg0 ]


{-| If you find yourself defining unique paddings all the time, you might consider defining

    edges =
        { top = 0
        , right = 0
        , bottom = 0
        , left = 0
        }

And then just do

    paddingEach { edges | right = 5 }

paddingEach: { top : Int, right : Int, bottom : Int, left : Int } -> Element.Attribute msg
-}
paddingEach :
    { top : Int, right : Int, bottom : Int, left : Int } -> Elm.Expression
paddingEach paddingEachArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "paddingEach"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "top", Type.int )
                            , ( "right", Type.int )
                            , ( "bottom", Type.int )
                            , ( "left", Type.int )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "top" (Elm.int paddingEachArg.top)
            , Tuple.pair "right" (Elm.int paddingEachArg.right)
            , Tuple.pair "bottom" (Elm.int paddingEachArg.bottom)
            , Tuple.pair "left" (Elm.int paddingEachArg.left)
            ]
        ]


{-| spacing: Int -> Element.Attribute msg -}
spacing : Int -> Elm.Expression
spacing spacingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "spacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int spacingArg ]


{-| In the majority of cases you'll just need to use `spacing`, which will work as intended.

However for some layouts, like `textColumn`, you may want to set a different spacing for the x axis compared to the y axis.

spacingXY: Int -> Int -> Element.Attribute msg
-}
spacingXY : Int -> Int -> Elm.Expression
spacingXY spacingXYArg spacingXYArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "spacingXY"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int spacingXYArg, Elm.int spacingXYArg0 ]


{-| spaceEvenly: Element.Attribute msg -}
spaceEvenly : Elm.Expression
spaceEvenly =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "spaceEvenly"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| centerX: Element.Attribute msg -}
centerX : Elm.Expression
centerX =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "centerX"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| centerY: Element.Attribute msg -}
centerY : Elm.Expression
centerY =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "centerY"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| alignLeft: Element.Attribute msg -}
alignLeft : Elm.Expression
alignLeft =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "alignLeft"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| alignRight: Element.Attribute msg -}
alignRight : Elm.Expression
alignRight =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "alignRight"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| alignTop: Element.Attribute msg -}
alignTop : Elm.Expression
alignTop =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "alignTop"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| alignBottom: Element.Attribute msg -}
alignBottom : Elm.Expression
alignBottom =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "alignBottom"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| Make an element transparent and have it ignore any mouse or touch events, though it will stil take up space.

transparent: Bool -> Element.Attr decorative msg
-}
transparent : Bool -> Elm.Expression
transparent transparentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "transparent"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool transparentArg ]


{-| A capped value between 0.0 and 1.0, where 0.0 is transparent and 1.0 is fully opaque.

Semantically equivalent to html opacity.

alpha: Float -> Element.Attr decorative msg
-}
alpha : Float -> Elm.Expression
alpha alphaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "alpha"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float alphaArg ]


{-| Set the cursor to be a pointing hand when it's hovering over this element.

pointer: Element.Attribute msg
-}
pointer : Elm.Expression
pointer =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "pointer"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| moveUp: Float -> Element.Attr decorative msg -}
moveUp : Float -> Elm.Expression
moveUp moveUpArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "moveUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float moveUpArg ]


{-| moveDown: Float -> Element.Attr decorative msg -}
moveDown : Float -> Elm.Expression
moveDown moveDownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "moveDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float moveDownArg ]


{-| moveRight: Float -> Element.Attr decorative msg -}
moveRight : Float -> Elm.Expression
moveRight moveRightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "moveRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float moveRightArg ]


{-| moveLeft: Float -> Element.Attr decorative msg -}
moveLeft : Float -> Elm.Expression
moveLeft moveLeftArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "moveLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float moveLeftArg ]


{-| Angle is given in radians. [Here are some conversion functions if you want to use another unit.](https://package.elm-lang.org/packages/elm/core/latest/Basics#degrees)

rotate: Float -> Element.Attr decorative msg
-}
rotate : Float -> Elm.Expression
rotate rotateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "rotate"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float rotateArg ]


{-| scale: Float -> Element.Attr decorative msg -}
scale : Float -> Elm.Expression
scale scaleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "scale"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float scaleArg ]


{-| clip: Element.Attribute msg -}
clip : Elm.Expression
clip =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "clip"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| clipX: Element.Attribute msg -}
clipX : Elm.Expression
clipX =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "clipX"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| clipY: Element.Attribute msg -}
clipY : Elm.Expression
clipY =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "clipY"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| scrollbars: Element.Attribute msg -}
scrollbars : Elm.Expression
scrollbars =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "scrollbars"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| scrollbarX: Element.Attribute msg -}
scrollbarX : Elm.Expression
scrollbarX =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "scrollbarX"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| scrollbarY: Element.Attribute msg -}
scrollbarY : Elm.Expression
scrollbarY =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "scrollbarY"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| This is your top level node where you can turn `Element` into `Html`.

layout: List (Element.Attribute msg) -> Element.Element msg -> Html.Html msg
-}
layout : List Elm.Expression -> Elm.Expression -> Elm.Expression
layout layoutArg layoutArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "layout"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.list layoutArg, layoutArg0 ]


{-| layoutWith: 
    { options : List Element.Option }
    -> List (Element.Attribute msg)
    -> Element.Element msg
    -> Html.Html msg
-}
layoutWith :
    { options : List Elm.Expression }
    -> List Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
layoutWith layoutWithArg layoutWithArg0 layoutWithArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "layoutWith"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "options"
                              , Type.list
                                    (Type.namedWith [ "Element" ] "Option" [])
                              )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.record [ Tuple.pair "options" (Elm.list layoutWithArg.options) ]
        , Elm.list layoutWithArg0
        , layoutWithArg1
        ]


{-| Elm UI embeds two StyleSheets, one that is constant, and one that changes dynamically based on styles collected from the elements being rendered.

This option will stop the static/constant stylesheet from rendering.

If you're embedding multiple elm-ui `layout` elements, you need to guarantee that only one is rendering the static style sheet and that it's above all the others in the DOM tree.

noStaticStyleSheet: Element.Option
-}
noStaticStyleSheet : Elm.Expression
noStaticStyleSheet =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "noStaticStyleSheet"
        , annotation = Just (Type.namedWith [ "Element" ] "Option" [])
        }


{-| Any `hover` styles, aka attributes with `mouseOver` in the name, will be always turned on.

This is useful for when you're targeting a platform that has no mouse, such as mobile.

forceHover: Element.Option
-}
forceHover : Elm.Expression
forceHover =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "forceHover"
        , annotation = Just (Type.namedWith [ "Element" ] "Option" [])
        }


{-| Disable all `mouseOver` styles.

noHover: Element.Option
-}
noHover : Elm.Expression
noHover =
    Elm.value
        { importFrom = [ "Element" ]
        , name = "noHover"
        , annotation = Just (Type.namedWith [ "Element" ] "Option" [])
        }


{-| focusStyle: Element.FocusStyle -> Element.Option -}
focusStyle : Elm.Expression -> Elm.Expression
focusStyle focusStyleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "focusStyle"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "FocusStyle" [] ]
                        (Type.namedWith [ "Element" ] "Option" [])
                    )
            }
        )
        [ focusStyleArg ]


{-| link []
        { url = "http://fruits.com"
        , label = text "A link to my favorite fruit provider."
        }

link: 
    List (Element.Attribute msg)
    -> { url : String, label : Element.Element msg }
    -> Element.Element msg
-}
link :
    List Elm.Expression
    -> { url : String, label : Elm.Expression }
    -> Elm.Expression
link linkArg linkArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "link"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "url", Type.string )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list linkArg
        , Elm.record
            [ Tuple.pair "url" (Elm.string linkArg0.url)
            , Tuple.pair "label" linkArg0.label
            ]
        ]


{-| newTabLink: 
    List (Element.Attribute msg)
    -> { url : String, label : Element.Element msg }
    -> Element.Element msg
-}
newTabLink :
    List Elm.Expression
    -> { url : String, label : Elm.Expression }
    -> Elm.Expression
newTabLink newTabLinkArg newTabLinkArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "newTabLink"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "url", Type.string )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list newTabLinkArg
        , Elm.record
            [ Tuple.pair "url" (Elm.string newTabLinkArg0.url)
            , Tuple.pair "label" newTabLinkArg0.label
            ]
        ]


{-| A link to download a file.

download: 
    List (Element.Attribute msg)
    -> { url : String, label : Element.Element msg }
    -> Element.Element msg
-}
download :
    List Elm.Expression
    -> { url : String, label : Elm.Expression }
    -> Elm.Expression
download downloadArg downloadArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "download"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "url", Type.string )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list downloadArg
        , Elm.record
            [ Tuple.pair "url" (Elm.string downloadArg0.url)
            , Tuple.pair "label" downloadArg0.label
            ]
        ]


{-| A link to download a file, but you can specify the filename.

downloadAs: 
    List (Element.Attribute msg)
    -> { label : Element.Element msg, filename : String, url : String }
    -> Element.Element msg
-}
downloadAs :
    List Elm.Expression
    -> { label : Elm.Expression, filename : String, url : String }
    -> Elm.Expression
downloadAs downloadAsArg downloadAsArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "downloadAs"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "label"
                              , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                              )
                            , ( "filename", Type.string )
                            , ( "url", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list downloadAsArg
        , Elm.record
            [ Tuple.pair "label" downloadAsArg0.label
            , Tuple.pair "filename" (Elm.string downloadAsArg0.filename)
            , Tuple.pair "url" (Elm.string downloadAsArg0.url)
            ]
        ]


{-| Both a source and a description are required for images.

The description is used for people using screen readers.

Leaving the description blank will cause the image to be ignored by assistive technology. This can make sense for images that are purely decorative and add no additional information.

So, take a moment to describe your image as you would to someone who has a harder time seeing.

image: 
    List (Element.Attribute msg)
    -> { src : String, description : String }
    -> Element.Element msg
-}
image :
    List Elm.Expression
    -> { src : String, description : String }
    -> Elm.Expression
image imageArg imageArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "image"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "src", Type.string )
                            , ( "description", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list imageArg
        , Elm.record
            [ Tuple.pair "src" (Elm.string imageArg0.src)
            , Tuple.pair "description" (Elm.string imageArg0.description)
            ]
        ]


{-| rgba: Float -> Float -> Float -> Float -> Element.Color -}
rgba : Float -> Float -> Float -> Float -> Elm.Expression
rgba rgbaArg rgbaArg0 rgbaArg1 rgbaArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "rgba"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float, Type.float, Type.float ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
        )
        [ Elm.float rgbaArg
        , Elm.float rgbaArg0
        , Elm.float rgbaArg1
        , Elm.float rgbaArg2
        ]


{-| Provide the red, green, and blue channels for the color.

Each channel takes a value between 0 and 1.

rgb: Float -> Float -> Float -> Element.Color
-}
rgb : Float -> Float -> Float -> Elm.Expression
rgb rgbArg rgbArg0 rgbArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "rgb"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float, Type.float ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
        )
        [ Elm.float rgbArg, Elm.float rgbArg0, Elm.float rgbArg1 ]


{-| Provide the red, green, and blue channels for the color.

Each channel takes a value between 0 and 255.

rgb255: Int -> Int -> Int -> Element.Color
-}
rgb255 : Int -> Int -> Int -> Elm.Expression
rgb255 rgb255Arg rgb255Arg0 rgb255Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "rgb255"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int, Type.int ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
        )
        [ Elm.int rgb255Arg, Elm.int rgb255Arg0, Elm.int rgb255Arg1 ]


{-| rgba255: Int -> Int -> Int -> Float -> Element.Color -}
rgba255 : Int -> Int -> Int -> Float -> Elm.Expression
rgba255 rgba255Arg rgba255Arg0 rgba255Arg1 rgba255Arg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "rgba255"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int, Type.int, Type.float ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
        )
        [ Elm.int rgba255Arg
        , Elm.int rgba255Arg0
        , Elm.int rgba255Arg1
        , Elm.float rgba255Arg2
        ]


{-| Create a color from an RGB record.

fromRgb: { red : Float, green : Float, blue : Float, alpha : Float } -> Element.Color
-}
fromRgb :
    { red : Float, green : Float, blue : Float, alpha : Float }
    -> Elm.Expression
fromRgb fromRgbArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "fromRgb"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "red", Type.float )
                            , ( "green", Type.float )
                            , ( "blue", Type.float )
                            , ( "alpha", Type.float )
                            ]
                        ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "red" (Elm.float fromRgbArg.red)
            , Tuple.pair "green" (Elm.float fromRgbArg.green)
            , Tuple.pair "blue" (Elm.float fromRgbArg.blue)
            , Tuple.pair "alpha" (Elm.float fromRgbArg.alpha)
            ]
        ]


{-| fromRgb255: { red : Int, green : Int, blue : Int, alpha : Float } -> Element.Color -}
fromRgb255 :
    { red : Int, green : Int, blue : Int, alpha : Float } -> Elm.Expression
fromRgb255 fromRgb255Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "fromRgb255"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "red", Type.int )
                            , ( "green", Type.int )
                            , ( "blue", Type.int )
                            , ( "alpha", Type.float )
                            ]
                        ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "red" (Elm.int fromRgb255Arg.red)
            , Tuple.pair "green" (Elm.int fromRgb255Arg.green)
            , Tuple.pair "blue" (Elm.int fromRgb255Arg.blue)
            , Tuple.pair "alpha" (Elm.float fromRgb255Arg.alpha)
            ]
        ]


{-| Deconstruct a `Color` into its rgb channels.

toRgb: Element.Color -> { red : Float, green : Float, blue : Float, alpha : Float }
-}
toRgb : Elm.Expression -> Elm.Expression
toRgb toRgbArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "toRgb"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Color" [] ]
                        (Type.record
                            [ ( "red", Type.float )
                            , ( "green", Type.float )
                            , ( "blue", Type.float )
                            , ( "alpha", Type.float )
                            ]
                        )
                    )
            }
        )
        [ toRgbArg ]


{-| above: Element.Element msg -> Element.Attribute msg -}
above : Elm.Expression -> Elm.Expression
above aboveArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "above"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ aboveArg ]


{-| below: Element.Element msg -> Element.Attribute msg -}
below : Elm.Expression -> Elm.Expression
below belowArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "below"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ belowArg ]


{-| onRight: Element.Element msg -> Element.Attribute msg -}
onRight : Elm.Expression -> Elm.Expression
onRight onRightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "onRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onRightArg ]


{-| onLeft: Element.Element msg -> Element.Attribute msg -}
onLeft : Elm.Expression -> Elm.Expression
onLeft onLeftArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "onLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onLeftArg ]


{-| This will place an element in front of another.

**Note:** If you use this on a `layout` element, it will place the element as fixed to the viewport which can be useful for modals and overlays.

inFront: Element.Element msg -> Element.Attribute msg
-}
inFront : Elm.Expression -> Elm.Expression
inFront inFrontArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "inFront"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ inFrontArg ]


{-| This will place an element between the background and the content of an element.

behindContent: Element.Element msg -> Element.Attribute msg
-}
behindContent : Elm.Expression -> Elm.Expression
behindContent behindContentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "behindContent"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ behindContentArg ]


{-| mouseOver: List Element.Decoration -> Element.Attribute msg -}
mouseOver : List Elm.Expression -> Elm.Expression
mouseOver mouseOverArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "mouseOver"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Element" ] "Decoration" [])
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list mouseOverArg ]


{-| mouseDown: List Element.Decoration -> Element.Attribute msg -}
mouseDown : List Elm.Expression -> Elm.Expression
mouseDown mouseDownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "mouseDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Element" ] "Decoration" [])
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list mouseDownArg ]


{-| focused: List Element.Decoration -> Element.Attribute msg -}
focused : List Elm.Expression -> Elm.Expression
focused focusedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "focused"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Element" ] "Decoration" [])
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list focusedArg ]


{-| Takes in a Window.Size and returns a device profile which can be used for responsiveness.

If you have more detailed concerns around responsiveness, it probably makes sense to copy this function into your codebase and modify as needed.

classifyDevice: { window | height : Int, width : Int } -> Element.Device
-}
classifyDevice : { window | height : Int, width : Int } -> Elm.Expression
classifyDevice classifyDeviceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "classifyDevice"
            , annotation =
                Just
                    (Type.function
                        [ Type.extensible
                            "window"
                            [ ( "height", Type.int ), ( "width", Type.int ) ]
                        ]
                        (Type.namedWith [ "Element" ] "Device" [])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "height" (Elm.int classifyDeviceArg.height)
            , Tuple.pair "width" (Elm.int classifyDeviceArg.width)
            ]
        ]


{-| When designing it's nice to use a modular scale to set spacial rythms.

    scaled =
        Element.modular 16 1.25

A modular scale starts with a number, and multiplies it by a ratio a number of times.
Then, when setting font sizes you can use:

    Font.size (scaled 1) -- results in 16

    Font.size (scaled 2) -- 16 * 1.25 results in 20

    Font.size (scaled 4) -- 16 * 1.25 ^ (4 - 1) results in 31.25

We can also provide negative numbers to scale below 16px.

    Font.size (scaled -1) -- 16 * 1.25 ^ (-1) results in 12.8

modular: Float -> Float -> Int -> Float
-}
modular : Float -> Float -> Int -> Elm.Expression
modular modularArg modularArg0 modularArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "modular"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float, Type.int ]
                        Type.float
                    )
            }
        )
        [ Elm.float modularArg, Elm.float modularArg0, Elm.int modularArg1 ]


{-| map: (msg -> msg1) -> Element.Element msg -> Element.Element msg1 -}
map : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
map mapArg mapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "msg" ] (Type.var "msg1")
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg1" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "mapUnpack" mapArg, mapArg0 ]


{-| mapAttribute: (msg -> msg1) -> Element.Attribute msg -> Element.Attribute msg1 -}
mapAttribute :
    (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
mapAttribute mapAttributeArg mapAttributeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "mapAttribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "msg" ] (Type.var "msg1")
                        , Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg1" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "mapAttributeUnpack" mapAttributeArg
        , mapAttributeArg0
        ]


{-| html: Html.Html msg -> Element.Element msg -}
html : Elm.Expression -> Elm.Expression
html htmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "html"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ htmlArg ]


{-| htmlAttribute: Html.Attribute msg -> Element.Attribute msg -}
htmlAttribute : Elm.Expression -> Elm.Expression
htmlAttribute htmlAttributeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element" ]
            , name = "htmlAttribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Html" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ htmlAttributeArg ]


annotation_ :
    { element : Type.Annotation -> Type.Annotation
    , column : Type.Annotation -> Type.Annotation -> Type.Annotation
    , indexedColumn : Type.Annotation -> Type.Annotation -> Type.Annotation
    , attribute : Type.Annotation -> Type.Annotation
    , length : Type.Annotation
    , option : Type.Annotation
    , focusStyle : Type.Annotation
    , color : Type.Annotation
    , attr : Type.Annotation -> Type.Annotation -> Type.Annotation
    , decoration : Type.Annotation
    , device : Type.Annotation
    , deviceClass : Type.Annotation
    , orientation : Type.Annotation
    }
annotation_ =
    { element =
        \elementArg0 ->
            Type.alias
                moduleName_
                "Element"
                [ elementArg0 ]
                (Type.namedWith
                    [ "Internal", "Model" ]
                    "Element"
                    [ Type.var "msg" ]
                )
    , column =
        \columnArg0 columnArg1 ->
            Type.alias
                moduleName_
                "Column"
                [ columnArg0, columnArg1 ]
                (Type.record
                    [ ( "header"
                      , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                      )
                    , ( "width", Type.namedWith [ "Element" ] "Length" [] )
                    , ( "view"
                      , Type.function
                            [ Type.var "record" ]
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                      )
                    ]
                )
    , indexedColumn =
        \indexedColumnArg0 indexedColumnArg1 ->
            Type.alias
                moduleName_
                "IndexedColumn"
                [ indexedColumnArg0, indexedColumnArg1 ]
                (Type.record
                    [ ( "header"
                      , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                      )
                    , ( "width", Type.namedWith [ "Element" ] "Length" [] )
                    , ( "view"
                      , Type.function
                            [ Type.int, Type.var "record" ]
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                      )
                    ]
                )
    , attribute =
        \attributeArg0 ->
            Type.alias
                moduleName_
                "Attribute"
                [ attributeArg0 ]
                (Type.namedWith
                    [ "Internal", "Model" ]
                    "Attribute"
                    [ Type.unit, Type.var "msg" ]
                )
    , length =
        Type.alias
            moduleName_
            "Length"
            []
            (Type.namedWith [ "Internal", "Model" ] "Length" [])
    , option =
        Type.alias
            moduleName_
            "Option"
            []
            (Type.namedWith [ "Internal", "Model" ] "Option" [])
    , focusStyle =
        Type.alias
            moduleName_
            "FocusStyle"
            []
            (Type.record
                [ ( "borderColor"
                  , Type.maybe (Type.namedWith [ "Element" ] "Color" [])
                  )
                , ( "backgroundColor"
                  , Type.maybe (Type.namedWith [ "Element" ] "Color" [])
                  )
                , ( "shadow"
                  , Type.maybe
                        (Type.record
                            [ ( "color"
                              , Type.namedWith [ "Element" ] "Color" []
                              )
                            , ( "offset", Type.tuple Type.int Type.int )
                            , ( "blur", Type.int )
                            , ( "size", Type.int )
                            ]
                        )
                  )
                ]
            )
    , color =
        Type.alias
            moduleName_
            "Color"
            []
            (Type.namedWith [ "Internal", "Model" ] "Color" [])
    , attr =
        \attrArg0 attrArg1 ->
            Type.alias
                moduleName_
                "Attr"
                [ attrArg0, attrArg1 ]
                (Type.namedWith
                    [ "Internal", "Model" ]
                    "Attribute"
                    [ Type.var "decorative", Type.var "msg" ]
                )
    , decoration =
        Type.alias
            moduleName_
            "Decoration"
            []
            (Type.namedWith
                [ "Internal", "Model" ]
                "Attribute"
                [ Type.namedWith [ "Basics" ] "Never" []
                , Type.namedWith [ "Basics" ] "Never" []
                ]
            )
    , device =
        Type.alias
            moduleName_
            "Device"
            []
            (Type.record
                [ ( "class", Type.namedWith [ "Element" ] "DeviceClass" [] )
                , ( "orientation"
                  , Type.namedWith [ "Element" ] "Orientation" []
                  )
                ]
            )
    , deviceClass = Type.namedWith [ "Element" ] "DeviceClass" []
    , orientation = Type.namedWith [ "Element" ] "Orientation" []
    }


make_ :
    { column :
        { header : Elm.Expression
        , width : Elm.Expression
        , view : Elm.Expression
        }
        -> Elm.Expression
    , indexedColumn :
        { header : Elm.Expression
        , width : Elm.Expression
        , view : Elm.Expression
        }
        -> Elm.Expression
    , focusStyle :
        { borderColor : Elm.Expression
        , backgroundColor : Elm.Expression
        , shadow : Elm.Expression
        }
        -> Elm.Expression
    , device :
        { class : Elm.Expression, orientation : Elm.Expression }
        -> Elm.Expression
    , phone : Elm.Expression
    , tablet : Elm.Expression
    , desktop : Elm.Expression
    , bigDesktop : Elm.Expression
    , portrait : Elm.Expression
    , landscape : Elm.Expression
    }
make_ =
    { column =
        \column_args ->
            Elm.withType
                (Type.alias
                    [ "Element" ]
                    "Column"
                    [ Type.var "record", Type.var "msg" ]
                    (Type.record
                        [ ( "header"
                          , Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                          )
                        , ( "width", Type.namedWith [ "Element" ] "Length" [] )
                        , ( "view"
                          , Type.function
                                [ Type.var "record" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "header" column_args.header
                    , Tuple.pair "width" column_args.width
                    , Tuple.pair "view" column_args.view
                    ]
                )
    , indexedColumn =
        \indexedColumn_args ->
            Elm.withType
                (Type.alias
                    [ "Element" ]
                    "IndexedColumn"
                    [ Type.var "record", Type.var "msg" ]
                    (Type.record
                        [ ( "header"
                          , Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                          )
                        , ( "width", Type.namedWith [ "Element" ] "Length" [] )
                        , ( "view"
                          , Type.function
                                [ Type.int, Type.var "record" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "header" indexedColumn_args.header
                    , Tuple.pair "width" indexedColumn_args.width
                    , Tuple.pair "view" indexedColumn_args.view
                    ]
                )
    , focusStyle =
        \focusStyle_args ->
            Elm.withType
                (Type.alias
                    [ "Element" ]
                    "FocusStyle"
                    []
                    (Type.record
                        [ ( "borderColor"
                          , Type.maybe (Type.namedWith [ "Element" ] "Color" [])
                          )
                        , ( "backgroundColor"
                          , Type.maybe (Type.namedWith [ "Element" ] "Color" [])
                          )
                        , ( "shadow"
                          , Type.maybe
                                (Type.record
                                    [ ( "color"
                                      , Type.namedWith [ "Element" ] "Color" []
                                      )
                                    , ( "offset", Type.tuple Type.int Type.int )
                                    , ( "blur", Type.int )
                                    , ( "size", Type.int )
                                    ]
                                )
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "borderColor" focusStyle_args.borderColor
                    , Tuple.pair
                        "backgroundColor"
                        focusStyle_args.backgroundColor
                    , Tuple.pair "shadow" focusStyle_args.shadow
                    ]
                )
    , device =
        \device_args ->
            Elm.withType
                (Type.alias
                    [ "Element" ]
                    "Device"
                    []
                    (Type.record
                        [ ( "class"
                          , Type.namedWith [ "Element" ] "DeviceClass" []
                          )
                        , ( "orientation"
                          , Type.namedWith [ "Element" ] "Orientation" []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "class" device_args.class
                    , Tuple.pair "orientation" device_args.orientation
                    ]
                )
    , phone =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "Phone"
            , annotation = Just (Type.namedWith [] "DeviceClass" [])
            }
    , tablet =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "Tablet"
            , annotation = Just (Type.namedWith [] "DeviceClass" [])
            }
    , desktop =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "Desktop"
            , annotation = Just (Type.namedWith [] "DeviceClass" [])
            }
    , bigDesktop =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "BigDesktop"
            , annotation = Just (Type.namedWith [] "DeviceClass" [])
            }
    , portrait =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "Portrait"
            , annotation = Just (Type.namedWith [] "Orientation" [])
            }
    , landscape =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "Landscape"
            , annotation = Just (Type.namedWith [] "Orientation" [])
            }
    }


caseOf_ :
    { deviceClass :
        Elm.Expression
        -> { deviceClassTags_0_0
            | phone : Elm.Expression
            , tablet : Elm.Expression
            , desktop : Elm.Expression
            , bigDesktop : Elm.Expression
        }
        -> Elm.Expression
    , orientation :
        Elm.Expression
        -> { orientationTags_1_0
            | portrait : Elm.Expression
            , landscape : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { deviceClass =
        \deviceClassExpression deviceClassTags ->
            Elm.Case.custom
                deviceClassExpression
                (Type.namedWith [ "Element" ] "DeviceClass" [])
                [ Elm.Case.branch0 "Phone" deviceClassTags.phone
                , Elm.Case.branch0 "Tablet" deviceClassTags.tablet
                , Elm.Case.branch0 "Desktop" deviceClassTags.desktop
                , Elm.Case.branch0 "BigDesktop" deviceClassTags.bigDesktop
                ]
    , orientation =
        \orientationExpression orientationTags ->
            Elm.Case.custom
                orientationExpression
                (Type.namedWith [ "Element" ] "Orientation" [])
                [ Elm.Case.branch0 "Portrait" orientationTags.portrait
                , Elm.Case.branch0 "Landscape" orientationTags.landscape
                ]
    }


call_ :
    { text : Elm.Expression -> Elm.Expression
    , el : Elm.Expression -> Elm.Expression -> Elm.Expression
    , row : Elm.Expression -> Elm.Expression -> Elm.Expression
    , wrappedRow : Elm.Expression -> Elm.Expression -> Elm.Expression
    , column : Elm.Expression -> Elm.Expression -> Elm.Expression
    , paragraph : Elm.Expression -> Elm.Expression -> Elm.Expression
    , textColumn : Elm.Expression -> Elm.Expression -> Elm.Expression
    , table : Elm.Expression -> Elm.Expression -> Elm.Expression
    , indexedTable : Elm.Expression -> Elm.Expression -> Elm.Expression
    , width : Elm.Expression -> Elm.Expression
    , height : Elm.Expression -> Elm.Expression
    , px : Elm.Expression -> Elm.Expression
    , fillPortion : Elm.Expression -> Elm.Expression
    , maximum : Elm.Expression -> Elm.Expression -> Elm.Expression
    , minimum : Elm.Expression -> Elm.Expression -> Elm.Expression
    , explain : Elm.Expression -> Elm.Expression
    , padding : Elm.Expression -> Elm.Expression
    , paddingXY : Elm.Expression -> Elm.Expression -> Elm.Expression
    , paddingEach : Elm.Expression -> Elm.Expression
    , spacing : Elm.Expression -> Elm.Expression
    , spacingXY : Elm.Expression -> Elm.Expression -> Elm.Expression
    , transparent : Elm.Expression -> Elm.Expression
    , alpha : Elm.Expression -> Elm.Expression
    , moveUp : Elm.Expression -> Elm.Expression
    , moveDown : Elm.Expression -> Elm.Expression
    , moveRight : Elm.Expression -> Elm.Expression
    , moveLeft : Elm.Expression -> Elm.Expression
    , rotate : Elm.Expression -> Elm.Expression
    , scale : Elm.Expression -> Elm.Expression
    , layout : Elm.Expression -> Elm.Expression -> Elm.Expression
    , layoutWith :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , focusStyle : Elm.Expression -> Elm.Expression
    , link : Elm.Expression -> Elm.Expression -> Elm.Expression
    , newTabLink : Elm.Expression -> Elm.Expression -> Elm.Expression
    , download : Elm.Expression -> Elm.Expression -> Elm.Expression
    , downloadAs : Elm.Expression -> Elm.Expression -> Elm.Expression
    , image : Elm.Expression -> Elm.Expression -> Elm.Expression
    , rgba :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , rgb : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , rgb255 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , rgba255 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , fromRgb : Elm.Expression -> Elm.Expression
    , fromRgb255 : Elm.Expression -> Elm.Expression
    , toRgb : Elm.Expression -> Elm.Expression
    , above : Elm.Expression -> Elm.Expression
    , below : Elm.Expression -> Elm.Expression
    , onRight : Elm.Expression -> Elm.Expression
    , onLeft : Elm.Expression -> Elm.Expression
    , inFront : Elm.Expression -> Elm.Expression
    , behindContent : Elm.Expression -> Elm.Expression
    , mouseOver : Elm.Expression -> Elm.Expression
    , mouseDown : Elm.Expression -> Elm.Expression
    , focused : Elm.Expression -> Elm.Expression
    , classifyDevice : Elm.Expression -> Elm.Expression
    , modular :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression
    , mapAttribute : Elm.Expression -> Elm.Expression -> Elm.Expression
    , html : Elm.Expression -> Elm.Expression
    , htmlAttribute : Elm.Expression -> Elm.Expression
    }
call_ =
    { text =
        \textArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "text"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textArg ]
    , el =
        \elArg elArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "el"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ elArg, elArg0 ]
    , row =
        \rowArg rowArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "row"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rowArg, rowArg0 ]
    , wrappedRow =
        \wrappedRowArg wrappedRowArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "wrappedRow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ wrappedRowArg, wrappedRowArg0 ]
    , column =
        \columnArg columnArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "column"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ columnArg, columnArg0 ]
    , paragraph =
        \paragraphArg paragraphArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "paragraph"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ paragraphArg, paragraphArg0 ]
    , textColumn =
        \textColumnArg textColumnArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "textColumn"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textColumnArg, textColumnArg0 ]
    , table =
        \tableArg tableArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "table"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "data", Type.list (Type.var "records") )
                                    , ( "columns"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Element" ]
                                                "Column"
                                                [ Type.var "records"
                                                , Type.var "msg"
                                                ]
                                            )
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tableArg, tableArg0 ]
    , indexedTable =
        \indexedTableArg indexedTableArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "indexedTable"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "data", Type.list (Type.var "records") )
                                    , ( "columns"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Element" ]
                                                "IndexedColumn"
                                                [ Type.var "records"
                                                , Type.var "msg"
                                                ]
                                            )
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ indexedTableArg, indexedTableArg0 ]
    , width =
        \widthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "width"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Element" ] "Length" [] ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ widthArg ]
    , height =
        \heightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "height"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Element" ] "Length" [] ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ heightArg ]
    , px =
        \pxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "px"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith [ "Element" ] "Length" [])
                            )
                    }
                )
                [ pxArg ]
    , fillPortion =
        \fillPortionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "fillPortion"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith [ "Element" ] "Length" [])
                            )
                    }
                )
                [ fillPortionArg ]
    , maximum =
        \maximumArg maximumArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "maximum"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.namedWith [ "Element" ] "Length" []
                                ]
                                (Type.namedWith [ "Element" ] "Length" [])
                            )
                    }
                )
                [ maximumArg, maximumArg0 ]
    , minimum =
        \minimumArg minimumArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "minimum"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.namedWith [ "Element" ] "Length" []
                                ]
                                (Type.namedWith [ "Element" ] "Length" [])
                            )
                    }
                )
                [ minimumArg, minimumArg0 ]
    , explain =
        \explainArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "explain"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Element" ] "Todo" [] ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ explainArg ]
    , padding =
        \paddingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "padding"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ paddingArg ]
    , paddingXY =
        \paddingXYArg paddingXYArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "paddingXY"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ paddingXYArg, paddingXYArg0 ]
    , paddingEach =
        \paddingEachArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "paddingEach"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "top", Type.int )
                                    , ( "right", Type.int )
                                    , ( "bottom", Type.int )
                                    , ( "left", Type.int )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ paddingEachArg ]
    , spacing =
        \spacingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "spacing"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ spacingArg ]
    , spacingXY =
        \spacingXYArg spacingXYArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "spacingXY"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ spacingXYArg, spacingXYArg0 ]
    , transparent =
        \transparentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "transparent"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ transparentArg ]
    , alpha =
        \alphaArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "alpha"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ alphaArg ]
    , moveUp =
        \moveUpArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "moveUp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ moveUpArg ]
    , moveDown =
        \moveDownArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "moveDown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ moveDownArg ]
    , moveRight =
        \moveRightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "moveRight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ moveRightArg ]
    , moveLeft =
        \moveLeftArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "moveLeft"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ moveLeftArg ]
    , rotate =
        \rotateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "rotate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rotateArg ]
    , scale =
        \scaleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "scale"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ scaleArg ]
    , layout =
        \layoutArg layoutArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "layout"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ layoutArg, layoutArg0 ]
    , layoutWith =
        \layoutWithArg layoutWithArg0 layoutWithArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "layoutWith"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "options"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Element" ]
                                                "Option"
                                                []
                                            )
                                      )
                                    ]
                                , Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ layoutWithArg, layoutWithArg0, layoutWithArg1 ]
    , focusStyle =
        \focusStyleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "focusStyle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Element" ] "FocusStyle" [] ]
                                (Type.namedWith [ "Element" ] "Option" [])
                            )
                    }
                )
                [ focusStyleArg ]
    , link =
        \linkArg linkArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "link"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "url", Type.string )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element" ]
                                            "Element"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ linkArg, linkArg0 ]
    , newTabLink =
        \newTabLinkArg newTabLinkArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "newTabLink"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "url", Type.string )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element" ]
                                            "Element"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ newTabLinkArg, newTabLinkArg0 ]
    , download =
        \downloadArg downloadArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "download"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "url", Type.string )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element" ]
                                            "Element"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ downloadArg, downloadArg0 ]
    , downloadAs =
        \downloadAsArg downloadAsArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "downloadAs"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "label"
                                      , Type.namedWith
                                            [ "Element" ]
                                            "Element"
                                            [ Type.var "msg" ]
                                      )
                                    , ( "filename", Type.string )
                                    , ( "url", Type.string )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ downloadAsArg, downloadAsArg0 ]
    , image =
        \imageArg imageArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "image"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "src", Type.string )
                                    , ( "description", Type.string )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ imageArg, imageArg0 ]
    , rgba =
        \rgbaArg rgbaArg0 rgbaArg1 rgbaArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "rgba"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float
                                , Type.float
                                , Type.float
                                , Type.float
                                ]
                                (Type.namedWith [ "Element" ] "Color" [])
                            )
                    }
                )
                [ rgbaArg, rgbaArg0, rgbaArg1, rgbaArg2 ]
    , rgb =
        \rgbArg rgbArg0 rgbArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "rgb"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float, Type.float ]
                                (Type.namedWith [ "Element" ] "Color" [])
                            )
                    }
                )
                [ rgbArg, rgbArg0, rgbArg1 ]
    , rgb255 =
        \rgb255Arg rgb255Arg0 rgb255Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "rgb255"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int, Type.int ]
                                (Type.namedWith [ "Element" ] "Color" [])
                            )
                    }
                )
                [ rgb255Arg, rgb255Arg0, rgb255Arg1 ]
    , rgba255 =
        \rgba255Arg rgba255Arg0 rgba255Arg1 rgba255Arg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "rgba255"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int, Type.int, Type.float ]
                                (Type.namedWith [ "Element" ] "Color" [])
                            )
                    }
                )
                [ rgba255Arg, rgba255Arg0, rgba255Arg1, rgba255Arg2 ]
    , fromRgb =
        \fromRgbArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "fromRgb"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "red", Type.float )
                                    , ( "green", Type.float )
                                    , ( "blue", Type.float )
                                    , ( "alpha", Type.float )
                                    ]
                                ]
                                (Type.namedWith [ "Element" ] "Color" [])
                            )
                    }
                )
                [ fromRgbArg ]
    , fromRgb255 =
        \fromRgb255Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "fromRgb255"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "red", Type.int )
                                    , ( "green", Type.int )
                                    , ( "blue", Type.int )
                                    , ( "alpha", Type.float )
                                    ]
                                ]
                                (Type.namedWith [ "Element" ] "Color" [])
                            )
                    }
                )
                [ fromRgb255Arg ]
    , toRgb =
        \toRgbArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "toRgb"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Element" ] "Color" [] ]
                                (Type.record
                                    [ ( "red", Type.float )
                                    , ( "green", Type.float )
                                    , ( "blue", Type.float )
                                    , ( "alpha", Type.float )
                                    ]
                                )
                            )
                    }
                )
                [ toRgbArg ]
    , above =
        \aboveArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "above"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ aboveArg ]
    , below =
        \belowArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "below"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ belowArg ]
    , onRight =
        \onRightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "onRight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onRightArg ]
    , onLeft =
        \onLeftArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "onLeft"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onLeftArg ]
    , inFront =
        \inFrontArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "inFront"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ inFrontArg ]
    , behindContent =
        \behindContentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "behindContent"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ behindContentArg ]
    , mouseOver =
        \mouseOverArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "mouseOver"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Decoration"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mouseOverArg ]
    , mouseDown =
        \mouseDownArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "mouseDown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Decoration"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mouseDownArg ]
    , focused =
        \focusedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "focused"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Decoration"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ focusedArg ]
    , classifyDevice =
        \classifyDeviceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "classifyDevice"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.extensible
                                    "window"
                                    [ ( "height", Type.int )
                                    , ( "width", Type.int )
                                    ]
                                ]
                                (Type.namedWith [ "Element" ] "Device" [])
                            )
                    }
                )
                [ classifyDeviceArg ]
    , modular =
        \modularArg modularArg0 modularArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "modular"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float, Type.int ]
                                Type.float
                            )
                    }
                )
                [ modularArg, modularArg0, modularArg1 ]
    , map =
        \mapArg mapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "map"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "msg" ]
                                    (Type.var "msg1")
                                , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg1" ]
                                )
                            )
                    }
                )
                [ mapArg, mapArg0 ]
    , mapAttribute =
        \mapAttributeArg mapAttributeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "mapAttribute"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "msg" ]
                                    (Type.var "msg1")
                                , Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg1" ]
                                )
                            )
                    }
                )
                [ mapAttributeArg, mapAttributeArg0 ]
    , html =
        \htmlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "html"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Html" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ htmlArg ]
    , htmlAttribute =
        \htmlAttributeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element" ]
                    , name = "htmlAttribute"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Html" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ htmlAttributeArg ]
    }


values_ :
    { none : Elm.Expression
    , text : Elm.Expression
    , el : Elm.Expression
    , row : Elm.Expression
    , wrappedRow : Elm.Expression
    , column : Elm.Expression
    , paragraph : Elm.Expression
    , textColumn : Elm.Expression
    , table : Elm.Expression
    , indexedTable : Elm.Expression
    , width : Elm.Expression
    , height : Elm.Expression
    , px : Elm.Expression
    , shrink : Elm.Expression
    , fill : Elm.Expression
    , fillPortion : Elm.Expression
    , maximum : Elm.Expression
    , minimum : Elm.Expression
    , explain : Elm.Expression
    , padding : Elm.Expression
    , paddingXY : Elm.Expression
    , paddingEach : Elm.Expression
    , spacing : Elm.Expression
    , spacingXY : Elm.Expression
    , spaceEvenly : Elm.Expression
    , centerX : Elm.Expression
    , centerY : Elm.Expression
    , alignLeft : Elm.Expression
    , alignRight : Elm.Expression
    , alignTop : Elm.Expression
    , alignBottom : Elm.Expression
    , transparent : Elm.Expression
    , alpha : Elm.Expression
    , pointer : Elm.Expression
    , moveUp : Elm.Expression
    , moveDown : Elm.Expression
    , moveRight : Elm.Expression
    , moveLeft : Elm.Expression
    , rotate : Elm.Expression
    , scale : Elm.Expression
    , clip : Elm.Expression
    , clipX : Elm.Expression
    , clipY : Elm.Expression
    , scrollbars : Elm.Expression
    , scrollbarX : Elm.Expression
    , scrollbarY : Elm.Expression
    , layout : Elm.Expression
    , layoutWith : Elm.Expression
    , noStaticStyleSheet : Elm.Expression
    , forceHover : Elm.Expression
    , noHover : Elm.Expression
    , focusStyle : Elm.Expression
    , link : Elm.Expression
    , newTabLink : Elm.Expression
    , download : Elm.Expression
    , downloadAs : Elm.Expression
    , image : Elm.Expression
    , rgba : Elm.Expression
    , rgb : Elm.Expression
    , rgb255 : Elm.Expression
    , rgba255 : Elm.Expression
    , fromRgb : Elm.Expression
    , fromRgb255 : Elm.Expression
    , toRgb : Elm.Expression
    , above : Elm.Expression
    , below : Elm.Expression
    , onRight : Elm.Expression
    , onLeft : Elm.Expression
    , inFront : Elm.Expression
    , behindContent : Elm.Expression
    , mouseOver : Elm.Expression
    , mouseDown : Elm.Expression
    , focused : Elm.Expression
    , classifyDevice : Elm.Expression
    , modular : Elm.Expression
    , map : Elm.Expression
    , mapAttribute : Elm.Expression
    , html : Elm.Expression
    , htmlAttribute : Elm.Expression
    }
values_ =
    { none =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "none"
            , annotation =
                Just (Type.namedWith [ "Element" ] "Element" [ Type.var "msg" ])
            }
    , text =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , el =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "el"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , row =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "row"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , wrappedRow =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "wrappedRow"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , column =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "column"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , paragraph =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "paragraph"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , textColumn =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "textColumn"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , table =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "table"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "data", Type.list (Type.var "records") )
                            , ( "columns"
                              , Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Column"
                                        [ Type.var "records", Type.var "msg" ]
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , indexedTable =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "indexedTable"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "data", Type.list (Type.var "records") )
                            , ( "columns"
                              , Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "IndexedColumn"
                                        [ Type.var "records", Type.var "msg" ]
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , width =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "width"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Length" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , height =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "height"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Length" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , px =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "px"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Element" ] "Length" [])
                    )
            }
    , shrink =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "shrink"
            , annotation = Just (Type.namedWith [ "Element" ] "Length" [])
            }
    , fill =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "fill"
            , annotation = Just (Type.namedWith [ "Element" ] "Length" [])
            }
    , fillPortion =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "fillPortion"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Element" ] "Length" [])
                    )
            }
    , maximum =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "maximum"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.namedWith [ "Element" ] "Length" [] ]
                        (Type.namedWith [ "Element" ] "Length" [])
                    )
            }
    , minimum =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "minimum"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.namedWith [ "Element" ] "Length" [] ]
                        (Type.namedWith [ "Element" ] "Length" [])
                    )
            }
    , explain =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "explain"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Todo" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , padding =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "padding"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , paddingXY =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "paddingXY"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , paddingEach =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "paddingEach"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "top", Type.int )
                            , ( "right", Type.int )
                            , ( "bottom", Type.int )
                            , ( "left", Type.int )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , spacing =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "spacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , spacingXY =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "spacingXY"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , spaceEvenly =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "spaceEvenly"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , centerX =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "centerX"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , centerY =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "centerY"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , alignLeft =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "alignLeft"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , alignRight =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "alignRight"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , alignTop =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "alignTop"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , alignBottom =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "alignBottom"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , transparent =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "transparent"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , alpha =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "alpha"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , pointer =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "pointer"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , moveUp =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "moveUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , moveDown =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "moveDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , moveRight =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "moveRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , moveLeft =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "moveLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , rotate =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "rotate"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , scale =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "scale"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , clip =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "clip"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , clipX =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "clipX"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , clipY =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "clipY"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , scrollbars =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "scrollbars"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , scrollbarX =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "scrollbarX"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , scrollbarY =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "scrollbarY"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , layout =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "layout"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , layoutWith =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "layoutWith"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "options"
                              , Type.list
                                    (Type.namedWith [ "Element" ] "Option" [])
                              )
                            ]
                        , Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , noStaticStyleSheet =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "noStaticStyleSheet"
            , annotation = Just (Type.namedWith [ "Element" ] "Option" [])
            }
    , forceHover =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "forceHover"
            , annotation = Just (Type.namedWith [ "Element" ] "Option" [])
            }
    , noHover =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "noHover"
            , annotation = Just (Type.namedWith [ "Element" ] "Option" [])
            }
    , focusStyle =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "focusStyle"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "FocusStyle" [] ]
                        (Type.namedWith [ "Element" ] "Option" [])
                    )
            }
    , link =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "link"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "url", Type.string )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , newTabLink =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "newTabLink"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "url", Type.string )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , download =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "download"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "url", Type.string )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , downloadAs =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "downloadAs"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "label"
                              , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                              )
                            , ( "filename", Type.string )
                            , ( "url", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , image =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "image"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "src", Type.string )
                            , ( "description", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , rgba =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "rgba"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float, Type.float, Type.float ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
    , rgb =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "rgb"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float, Type.float ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
    , rgb255 =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "rgb255"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int, Type.int ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
    , rgba255 =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "rgba255"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int, Type.int, Type.float ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
    , fromRgb =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "fromRgb"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "red", Type.float )
                            , ( "green", Type.float )
                            , ( "blue", Type.float )
                            , ( "alpha", Type.float )
                            ]
                        ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
    , fromRgb255 =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "fromRgb255"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "red", Type.int )
                            , ( "green", Type.int )
                            , ( "blue", Type.int )
                            , ( "alpha", Type.float )
                            ]
                        ]
                        (Type.namedWith [ "Element" ] "Color" [])
                    )
            }
    , toRgb =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "toRgb"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Color" [] ]
                        (Type.record
                            [ ( "red", Type.float )
                            , ( "green", Type.float )
                            , ( "blue", Type.float )
                            , ( "alpha", Type.float )
                            ]
                        )
                    )
            }
    , above =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "above"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , below =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "below"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onRight =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "onRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onLeft =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "onLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , inFront =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "inFront"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , behindContent =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "behindContent"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mouseOver =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "mouseOver"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Element" ] "Decoration" [])
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mouseDown =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "mouseDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Element" ] "Decoration" [])
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , focused =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "focused"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Element" ] "Decoration" [])
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , classifyDevice =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "classifyDevice"
            , annotation =
                Just
                    (Type.function
                        [ Type.extensible
                            "window"
                            [ ( "height", Type.int ), ( "width", Type.int ) ]
                        ]
                        (Type.namedWith [ "Element" ] "Device" [])
                    )
            }
    , modular =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "modular"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float, Type.int ]
                        Type.float
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "msg" ] (Type.var "msg1")
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg1" ]
                        )
                    )
            }
    , mapAttribute =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "mapAttribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "msg" ] (Type.var "msg1")
                        , Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg1" ]
                        )
                    )
            }
    , html =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "html"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , htmlAttribute =
        Elm.value
            { importFrom = [ "Element" ]
            , name = "htmlAttribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Html" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


