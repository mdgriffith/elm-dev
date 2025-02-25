module Generate.Docs exposing (generate)

{-| -}

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Type
import Elm.Package
import Elm.Project
import Elm.Version
import Generate.Docs.Module
import Options.Docs


generate : Options.Docs.Docs -> List Elm.File
generate docs =
    List.concat
        [ [ generateProject docs
          , generateGuides docs
          , generateModules docs
          ]
        , generatePackages docs
        ]


generateProject : Options.Docs.Docs -> Elm.File
generateProject docs =
    Elm.file [ "Docs", "Project" ]
        [ Elm.declaration "project"
            (genProject docs.project)
        , Elm.customType "Project"
            [ Elm.variantWith "App"
                [ Type.record
                    [ ( "dirs", Type.list Type.string )
                    , ( "depsDirect", Type.list versionName )
                    , ( "depsIndirect", Type.list versionName )
                    , ( "testDepsDirect", Type.list versionName )
                    , ( "testDepsIndirect", Type.list versionName )
                    ]
                ]
            , Elm.variantWith "Package"
                [ Type.record
                    [ ( "name", Type.string )
                    , ( "summary", Type.string )
                    , ( "version", versionType )
                    ]
                ]
            ]
        ]


versionName : Type.Annotation
versionName =
    Type.record
        [ ( "name", Type.string )
        , ( "version", versionType )
        ]


versionType : Type.Annotation
versionType =
    Type.triple Type.int Type.int Type.int


{-|

    type Project
        = Application ApplicationInfo
        | Package PackageInfo

-}
genProject : Elm.Project.Project -> Elm.Expression
genProject project =
    case project of
        Elm.Project.Application app ->
            Elm.apply (Elm.val "App") [ genApplication app ]
                |> Elm.withType (Type.named [] "Project")

        Elm.Project.Package pkg ->
            Elm.apply (Elm.val "Package") [ genPackage pkg ]
                |> Elm.withType (Type.named [] "Project")


{-|

    { name = Name
    , summary = String
    , license = License
    , version = Version
    , exposed = Exposed
    , deps = Deps Constraint
    , testDeps = Deps Constraint
    , elm = Constraint
    }

-}
genPackage : Elm.Project.PackageInfo -> Elm.Expression
genPackage pkg =
    Elm.record
        [ ( "name", Elm.string (Elm.Package.toString pkg.name) )
        , ( "summary", Elm.string pkg.summary )
        , ( "version", genVersion pkg.version )
        ]


{-|

    { elm = Version
    , dirs = List String
    , depsDirect = Deps Version
    , depsIndirect = Deps Version
    , testDepsDirect = Deps Version
    , testDepsIndirect = Deps Version
    }

-}
genApplication : Elm.Project.ApplicationInfo -> Elm.Expression
genApplication app =
    Elm.record
        [ ( "dirs", Elm.list (List.map Elm.string app.dirs) )
        , ( "depsDirect", Elm.list (List.map fromDeps app.depsDirect) )
        , ( "depsIndirect", Elm.list (List.map fromDeps app.depsIndirect) )
        , ( "testDepsDirect", Elm.list (List.map fromDeps app.testDepsDirect) )
        , ( "testDepsIndirect", Elm.list (List.map fromDeps app.testDepsIndirect) )
        ]


fromDeps : ( Elm.Package.Name, Elm.Version.Version ) -> Elm.Expression
fromDeps ( name, version ) =
    Elm.record
        [ ( "name", Elm.string (Elm.Package.toString name) )
        , ( "version", genVersion version )
        ]


genVersion : Elm.Version.Version -> Elm.Expression
genVersion version =
    let
        ( major, minor, patch ) =
            Elm.Version.toTuple version
    in
    Elm.triple (Elm.int major) (Elm.int minor) (Elm.int patch)


generateGuides : Options.Docs.Docs -> Elm.File
generateGuides docs =
    Elm.file [ "Docs", "Guides" ]
        [ Elm.declaration "all_"
            (Elm.list
                (List.filterMap
                    (\guide ->
                        case guide.content of
                            Nothing ->
                                Nothing

                            Just content ->
                                Just <|
                                    Elm.record
                                        [ ( "path", Elm.string guide.name )
                                        , ( "content", Elm.string (String.replace "\\" "\\\\" content) )
                                        ]
                    )
                    docs.guides
                )
                |> Elm.withType
                    (Type.list
                        (Type.record
                            [ ( "path", Type.string )
                            , ( "content", Type.string )
                            ]
                        )
                    )
            )
        ]


generateModules : Options.Docs.Docs -> Elm.File
generateModules docs =
    Elm.file [ "Docs", "Modules" ]
        [ Elm.declaration "modules"
            (Elm.list
                (List.map
                    Generate.Docs.Module.generate
                    docs.modules
                )
                |> Elm.withType (Type.list (Type.named [ "Elm", "Docs" ] "Module"))
            )
            |> Elm.expose
        ]


generatePackages : Options.Docs.Docs -> List Elm.File
generatePackages docs =
    case Dict.toList docs.deps of
        [] ->
            []

        pkgs ->
            let
                toPackage ( packageName, mods ) =
                    Elm.record
                        [ ( "name", Elm.string packageName )
                        , ( "modules"
                          , Elm.value
                                { importFrom = [ "Docs", "Packages", sanitizePackageName packageName ]
                                , name = "info"
                                , annotation = Just (Type.list (Type.named [ "Elm", "Docs" ] "Module"))
                                }
                          )
                        ]

                directory =
                    Elm.file [ "Docs", "Packages" ]
                        [ Elm.declaration "directory"
                            (Elm.list (List.map toPackage pkgs))
                        ]
            in
            directory
                :: List.map
                    (\( name, mods ) ->
                        Elm.file [ "Docs", "Packages", sanitizePackageName name ]
                            [ Elm.declaration "info"
                                (Elm.list (List.map Generate.Docs.Module.generate mods))
                            ]
                    )
                    pkgs


sanitizePackageName : String -> String
sanitizePackageName name =
    String.replace "." "_" name
        |> String.replace "-" "_"
        |> String.replace "/" "_"
        |> String.split "_"
        |> List.map capitalize
        |> String.join ""


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain


decapitalize : String -> String
decapitalize str =
    case String.uncons str of
        Nothing ->
            str

        Just ( first, tail ) ->
            String.fromChar (Char.toLower first) ++ tail
