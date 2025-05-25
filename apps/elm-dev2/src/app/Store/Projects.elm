module Store.Projects exposing
    ( store
    , Model, Msg(..)
    , Project
    , Status(..), Text(..), Color(..)
    )

{-|

@docs store

@docs Model, Msg

@docs Project

@docs Status, Text, Color

-}

import App.Store
import Data.Editor
import Dict
import Effect
import Elm.Module
import Elm.Project
import Json.Decode
import Json.Encode
import Listen


type alias Path =
    String


type alias Model =
    { current : Maybe Path
    , projects : Dict.Dict Path Project
    }


type alias Project =
    { root : Path
    , projectRoot : String
    , status : Status
    , entrypoints : List String
    , info : Elm.Project.ApplicationInfo
    , localModules : List Elm.Module.Name
    }


type Status
    = NoData
    | Success
    | GlobalError GlobalErrorDetails
    | CompilerError
        { errors : List File
        }


type alias GlobalErrorDetails =
    { path : Maybe String
    , problem :
        { title : String
        , message : List Text
        }
    }


type alias File =
    { path : String
    , name : String
    , problem : List Problem
    }


type alias Problem =
    { title : String
    , message : List Text
    , region : Data.Editor.Region
    }


type Text
    = Plain String
    | Styled StyledText String
      -- This is when we're quoting code from the actual elm file
    | CodeQuote Int (List Text)
      -- This is when we're referencing new code
      -- Like "did you mean xyz?"
    | CodeSection Int (List Text)


type alias StyledText =
    { color : Maybe Color
    , underline : Bool
    , bold : Bool
    }


type Color
    = Red
    | Yellow
    | Green
    | Cyan


type ErrType
    = Single
    | Many



-- Update/Msg


type Msg
    = ProjectReceived Project
    | ProjectSelected Path


store : App.Store.Store Msg Model
store =
    App.Store.store
        { init =
            \flags url maybeCachedModel ->
                let
                    model =
                        -- `maybeCachedModel` is the model from localstorage
                        -- If `App.Store.withLocalStorage` is defined
                        -- and it's available
                        maybeCachedModel
                            |> Maybe.withDefault
                                { current = Nothing
                                , projects = Dict.empty
                                }
                in
                ( model
                , Effect.none
                )
        , update =
            \msg model ->
                case msg of
                    ProjectReceived project ->
                        ( { model
                            | projects = Dict.insert project.root project model.projects
                            , current =
                                case model.current of
                                    Just current ->
                                        model.current

                                    Nothing ->
                                        Just project.root
                          }
                        , Effect.none
                        )

                    ProjectSelected path ->
                        ( { model | current = Just path }
                        , Effect.none
                        )
        , subscriptions = \_ -> Listen.none
        }
        |> App.Store.withLocalStorage
            { decoder = decoder
            , encode = encode
            }


encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "current"
          , case model.current of
                Nothing ->
                    Json.Encode.null

                Just current ->
                    Json.Encode.string current
          )
        , ( "projects", Json.Encode.dict identity encodeProject model.projects )
        ]


encodeProject : Project -> Json.Encode.Value
encodeProject project =
    Json.Encode.object
        [ ( "root", Json.Encode.string project.root )
        , ( "info", Elm.Project.encode (Elm.Project.Application project.info) )
        , ( "localModules", Json.Encode.list Elm.Module.encode project.localModules )
        ]


decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.map2 Model
        (Json.Decode.field "current" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "projects" (Json.Decode.dict projectDecoder))


projectDecoder : Json.Decode.Decoder Project
projectDecoder =
    Json.Decode.map6 Project
        (Json.Decode.field "root" Json.Decode.string)
        (Json.Decode.field "projectRoot" Json.Decode.string)
        (Json.Decode.field "status" decodeStatus)
        (Json.Decode.field "entrypoints" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "info" applicationInfoDecoder)
        (Json.Decode.field "localModules" (Json.Decode.list Elm.Module.decoder))


applicationInfoDecoder : Json.Decode.Decoder Elm.Project.ApplicationInfo
applicationInfoDecoder =
    Elm.Project.decoder
        |> Json.Decode.andThen
            (\project ->
                case project of
                    Elm.Project.Application info ->
                        Json.Decode.succeed info

                    _ ->
                        Json.Decode.fail "Expected an application project"
            )


decodeStatus : Json.Decode.Decoder Status
decodeStatus =
    Json.Decode.oneOf
        [ Json.Decode.field "compiled" Json.Decode.bool
            |> Json.Decode.map (\_ -> Success)
        , Json.Decode.field "type" decodeErrorType
            |> Json.Decode.andThen
                (\errorType ->
                    case errorType of
                        Single ->
                            Json.Decode.map GlobalError
                                (Json.Decode.map3
                                    (\path title message ->
                                        GlobalErrorDetails path
                                            { title = title
                                            , message = message
                                            }
                                    )
                                    (Json.Decode.field "path"
                                        (Json.Decode.nullable Json.Decode.string)
                                    )
                                    (Json.Decode.field "title" Json.Decode.string)
                                    (Json.Decode.field "message" (Json.Decode.list text))
                                )

                        Many ->
                            Json.Decode.map
                                (\err ->
                                    CompilerError
                                        { errors = err
                                        }
                                )
                                (Json.Decode.field "errors" (Json.Decode.list fileError))
                )
        ]


decodeErrorType : Json.Decode.Decoder ErrType
decodeErrorType =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "error" ->
                        Json.Decode.succeed Single

                    "compile-errors" ->
                        Json.Decode.succeed Many

                    _ ->
                        Json.Decode.fail ("Unsupported error type: " ++ str)
            )


fileError : Json.Decode.Decoder File
fileError =
    Json.Decode.map3 File
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "problems"
            (Json.Decode.map
                (List.sortBy (.region >> .start >> .line))
                (Json.Decode.list decodeProblem)
            )
        )


decodeProblem : Json.Decode.Decoder Problem
decodeProblem =
    Json.Decode.map3 Problem
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "message"
            (Json.Decode.map nestCodingSections (Json.Decode.list text))
        )
        (Json.Decode.field "region" Data.Editor.decodeRegion)


text : Json.Decode.Decoder Text
text =
    Json.Decode.oneOf
        [ Json.Decode.map Plain Json.Decode.string
        , Json.Decode.map2 Styled
            styledText
            (Json.Decode.field "string" Json.Decode.string)
        ]


styledText : Json.Decode.Decoder StyledText
styledText =
    Json.Decode.map3 StyledText
        (Json.Decode.field "color" maybeColor)
        (Json.Decode.field "underline" Json.Decode.bool)
        (Json.Decode.field "bold" Json.Decode.bool)


maybeColor : Json.Decode.Decoder (Maybe Color)
maybeColor =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\val ->
                    case String.toUpper val of
                        "YELLOW" ->
                            Json.Decode.succeed (Just Yellow)

                        "RED" ->
                            Json.Decode.succeed (Just Red)

                        "CYAN" ->
                            Json.Decode.succeed (Just Cyan)

                        "GREEN" ->
                            Json.Decode.succeed (Just Green)

                        "" ->
                            Json.Decode.succeed Nothing

                        _ ->
                            Json.Decode.fail ("Unknown Color: " ++ val)
                )
        , Json.Decode.null Nothing
        ]


type Capture
    = NoCapture
    | Quote (List Text)
    | Reference (List Text)


nestCodingSections : List Text -> List Text
nestCodingSections texts =
    nestCodingSectionsHelper texts NoCapture []


nestCodingSectionsHelper texts gatheredCodeRegion gathered =
    case texts of
        [] ->
            case gatheredCodeRegion of
                NoCapture ->
                    List.reverse gathered

                Quote codeRegion ->
                    let
                        ( reversed, lineCount ) =
                            reverseAndCount codeRegion
                    in
                    List.reverse (CodeQuote lineCount reversed :: gathered)

                Reference codeRegion ->
                    let
                        ( reversed, lineCount ) =
                            reverseAndCount codeRegion
                    in
                    List.reverse (CodeSection lineCount reversed :: gathered)

        (Plain txt) :: remaining ->
            let
                ( newCodeRegion, newGathered ) =
                    gatherCodeSectionRecurse Plain
                        txt
                        gatheredCodeRegion
                        gathered
            in
            nestCodingSectionsHelper remaining newCodeRegion newGathered

        (Styled style txt) :: remaining ->
            let
                ( newCodeRegion, newGathered ) =
                    gatherCodeSectionRecurse (Styled style)
                        txt
                        gatheredCodeRegion
                        gathered
            in
            nestCodingSectionsHelper remaining newCodeRegion newGathered

        (CodeSection lineCount txt) :: remaining ->
            -- This branch generally shouldn't happen because we haven't built code sections yet
            nestCodingSectionsHelper remaining gatheredCodeRegion (CodeSection lineCount txt :: gathered)

        (CodeQuote lineCount txt) :: remaining ->
            -- This branch generally shouldn't happen because we haven't built code sections yet
            nestCodingSectionsHelper remaining gatheredCodeRegion (CodeQuote lineCount txt :: gathered)


reverseAndCount : List a -> ( List a, number )
reverseAndCount items =
    reverseAndCountHelper items ( [], 0 )


reverseAndCountHelper items ( reved, count ) =
    case items of
        [] ->
            ( reved, count )

        top :: remain ->
            reverseAndCountHelper remain ( top :: reved, count + 1 )


gatherCodeSectionRecurse : (String -> Text) -> String -> Capture -> List Text -> ( Capture, List Text )
gatherCodeSectionRecurse toText str capture accum =
    gatherCodeSectionRecurseHelper toText (String.lines str) True capture accum


{-| -}
gatherCodeSectionRecurseHelper toText lines isFirst capture accum =
    case lines of
        [] ->
            ( capture, accum )

        [ last ] ->
            if isFirst then
                -- there were no newlines
                -- just add normally
                case capture of
                    NoCapture ->
                        ( NoCapture
                        , toText last :: accum
                        )

                    Quote reg ->
                        ( Quote (toText last :: reg)
                        , accum
                        )

                    Reference reg ->
                        ( Reference (toText last :: reg)
                        , accum
                        )

            else
                let
                    line =
                        "\n" ++ last
                in
                -- there were newlines previously
                if startsWithNum last || startsWithWs last then
                    case capture of
                        NoCapture ->
                            ( if startsWithWs last then
                                Reference [ toText last ]

                              else
                                Quote [ toText last ]
                            , toText "\n" :: accum
                            )

                        Quote reg ->
                            ( Quote (toText line :: reg)
                            , accum
                            )

                        Reference reg ->
                            ( Reference (toText line :: reg)
                            , accum
                            )

                else
                    case capture of
                        NoCapture ->
                            ( NoCapture
                            , toText line :: accum
                            )

                        Quote items ->
                            let
                                ( reversed, lineCount ) =
                                    reverseAndCount items
                            in
                            ( NoCapture
                            , toText line :: CodeQuote lineCount reversed :: accum
                            )

                        Reference items ->
                            let
                                ( reversed, lineCount ) =
                                    reverseAndCount items
                            in
                            ( NoCapture
                            , toText line :: CodeSection lineCount reversed :: accum
                            )

        topLine :: remainingLines ->
            if isFirst then
                -- there were no newlines
                -- just add normally
                case capture of
                    NoCapture ->
                        gatherCodeSectionRecurseHelper toText
                            remainingLines
                            False
                            NoCapture
                            (toText topLine :: accum)

                    Quote reg ->
                        gatherCodeSectionRecurseHelper toText
                            remainingLines
                            False
                            (Quote (toText topLine :: reg))
                            accum

                    Reference reg ->
                        gatherCodeSectionRecurseHelper toText
                            remainingLines
                            False
                            (Reference (toText topLine :: reg))
                            accum

            else
                let
                    line =
                        "\n" ++ topLine
                in
                -- there were newlines previously
                if startsWithNum topLine || startsWithWs topLine then
                    case capture of
                        NoCapture ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                (if startsWithWs topLine then
                                    Reference [ toText topLine ]

                                 else
                                    Quote [ toText topLine ]
                                )
                                (toText "\n" :: accum)

                        Quote reg ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                (Quote (toText line :: reg))
                                accum

                        Reference reg ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                (Reference (toText line :: reg))
                                accum

                else
                    case capture of
                        NoCapture ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                NoCapture
                                (toText line :: accum)

                        Quote items ->
                            let
                                ( reversed, lineCount ) =
                                    reverseAndCount items
                            in
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                NoCapture
                                (toText line :: CodeQuote lineCount reversed :: accum)

                        Reference items ->
                            let
                                ( reversed, lineCount ) =
                                    reverseAndCount items
                            in
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                NoCapture
                                (toText line :: CodeSection lineCount reversed :: accum)


startsWithNum : String -> Bool
startsWithNum str =
    case String.uncons (String.left 1 str) of
        Nothing ->
            False

        Just ( top, _ ) ->
            Char.isDigit top


startsWithWs : String -> Bool
startsWithWs str =
    case String.uncons (String.left 1 str) of
        Nothing ->
            False

        Just ( top, _ ) ->
            top == ' '
