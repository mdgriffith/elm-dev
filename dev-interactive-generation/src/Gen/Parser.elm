module Gen.Parser exposing (andThen, annotation_, backtrackable, call_, caseOf_, chompIf, chompUntil, chompUntilEndOr, chompWhile, commit, deadEndsToString, end, float, getChompedString, getCol, getIndent, getOffset, getPosition, getRow, getSource, int, keyword, lazy, lineComment, loop, make_, map, mapChompedString, moduleName_, multiComment, number, oneOf, problem, run, sequence, spaces, succeed, symbol, token, values_, variable, withIndent)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, getSource, getOffset, getCol, getRow, getPosition, getIndent, withIndent, deadEndsToString, mapChompedString, chompUntilEndOr, chompUntil, chompWhile, chompIf, getChompedString, multiComment, lineComment, spaces, loop, sequence, token, commit, backtrackable, map, oneOf, problem, andThen, lazy, succeed, end, variable, keyword, symbol, number, float, int, run, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Parser" ]


{-| Try a parser. Here are some examples using the [`keyword`](#keyword)
parser:

    run (keyword "true") "true"  == Ok ()
    run (keyword "true") "True"  == Err ...
    run (keyword "true") "false" == Err ...
    run (keyword "true") "true!" == Ok ()

Notice the last case! A `Parser` will chomp as much as possible and not worry
about the rest. Use the [`end`](#end) parser to ensure you made it to the end
of the string!

run: Parser.Parser a -> String -> Result.Result (List Parser.DeadEnd) a
-}
run : Elm.Expression -> String -> Elm.Expression
run runArg runArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "run"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        , Type.string
                        ]
                        (Type.namedWith
                            [ "Result" ]
                            "Result"
                            [ Type.list
                                (Type.namedWith [ "Parser" ] "DeadEnd" [])
                            , Type.var "a"
                            ]
                        )
                    )
            }
        )
        [ runArg, Elm.string runArg0 ]


{-| Parse integers.

    run int "1"    == Ok 1
    run int "1234" == Ok 1234

    run int "-789" == Err ...
    run int "0123" == Err ...
    run int "1.34" == Err ...
    run int "1e31" == Err ...
    run int "123a" == Err ...
    run int "0x1A" == Err ...

If you want to handle a leading `+` or `-` you should do it with a custom
parser like this:

    myInt : Parser Int
    myInt =
      oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]

**Note:** If you want a parser for both `Int` and `Float` literals, check out
[`number`](#number) below. It will be faster than using `oneOf` to combining
`int` and `float` yourself.

int: Parser.Parser Int
-}
int : Elm.Expression
int =
    Elm.value
        { importFrom = [ "Parser" ]
        , name = "int"
        , annotation = Just (Type.namedWith [ "Parser" ] "Parser" [ Type.int ])
        }


{-| Parse floats.

    run float "123"       == Ok 123
    run float "3.1415"    == Ok 3.1415
    run float "0.1234"    == Ok 0.1234
    run float ".1234"     == Ok 0.1234
    run float "1e-42"     == Ok 1e-42
    run float "6.022e23"  == Ok 6.022e23
    run float "6.022E23"  == Ok 6.022e23
    run float "6.022e+23" == Ok 6.022e23

If you want to disable literals like `.123` (like in Elm) you could write
something like this:

    elmFloat : Parser Float
    elmFloat =
      oneOf
        [ symbol "."
            |. problem "floating point numbers must start with a digit, like 0.25"
        , float
        ]

**Note:** If you want a parser for both `Int` and `Float` literals, check out
[`number`](#number) below. It will be faster than using `oneOf` to combining
`int` and `float` yourself.

float: Parser.Parser Float
-}
float : Elm.Expression
float =
    Elm.value
        { importFrom = [ "Parser" ]
        , name = "float"
        , annotation =
            Just (Type.namedWith [ "Parser" ] "Parser" [ Type.float ])
        }


{-| Parse a bunch of different kinds of numbers without backtracking. A parser
for Elm would need to handle integers, floats, and hexadecimal like this:

    type Expr
      = Variable String
      | Int Int
      | Float Float
      | Apply Expr Expr

    elmNumber : Parser Expr
    elmNumber =
      number
        { int = Just Int
        , hex = Just Int    -- 0x001A is allowed
        , octal = Nothing   -- 0o0731 is not
        , binary = Nothing  -- 0b1101 is not
        , float = Just Float
        }

If you wanted to implement the [`float`](#float) parser, it would be like this:

    float : Parser Float
    float =
      number
        { int = Just toFloat
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just identity
        }

Notice that it actually is processing `int` results! This is because `123`
looks like an integer to me, but maybe it looks like a float to you. If you had
`int = Nothing`, floats would need a decimal like `1.0` in every case. If you
like explicitness, that may actually be preferable!

**Note:** This function does not check for weird trailing characters in the
current implementation, so parsing `123abc` can succeed up to `123` and then
move on. This is helpful for people who want to parse things like `40px` or
`3m`, but it requires a bit of extra code to rule out trailing characters in
other cases.

number: 
    { int : Maybe (Int -> a)
    , hex : Maybe (Int -> a)
    , octal : Maybe (Int -> a)
    , binary : Maybe (Int -> a)
    , float : Maybe (Float -> a)
    }
    -> Parser.Parser a
-}
number :
    { int : Elm.Expression
    , hex : Elm.Expression
    , octal : Elm.Expression
    , binary : Elm.Expression
    , float : Elm.Expression
    }
    -> Elm.Expression
number numberArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "number"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "int"
                              , Type.maybe
                                    (Type.function [ Type.int ] (Type.var "a"))
                              )
                            , ( "hex"
                              , Type.maybe
                                    (Type.function [ Type.int ] (Type.var "a"))
                              )
                            , ( "octal"
                              , Type.maybe
                                    (Type.function [ Type.int ] (Type.var "a"))
                              )
                            , ( "binary"
                              , Type.maybe
                                    (Type.function [ Type.int ] (Type.var "a"))
                              )
                            , ( "float"
                              , Type.maybe
                                    (Type.function [ Type.float ] (Type.var "a")
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "int" numberArg.int
            , Tuple.pair "hex" numberArg.hex
            , Tuple.pair "octal" numberArg.octal
            , Tuple.pair "binary" numberArg.binary
            , Tuple.pair "float" numberArg.float
            ]
        ]


{-| Parse symbols like `(` and `,`.

    run (symbol "[") "[" == Ok ()
    run (symbol "[") "4" == Err ... (ExpectingSymbol "[") ...

**Note:** This is good for stuff like brackets and semicolons, but it probably
should not be used for binary operators like `+` and `-` because you can find
yourself in weird situations. For example, is `3--4` a typo? Or is it `3 - -4`?
I have had better luck with `chompWhile isSymbol` and sorting out which
operator it is afterwards.

symbol: String -> Parser.Parser ()
-}
symbol : String -> Elm.Expression
symbol symbolArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "symbol"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
        )
        [ Elm.string symbolArg ]


{-| Parse keywords like `let`, `case`, and `type`.

    run (keyword "let") "let"     == Ok ()
    run (keyword "let") "var"     == Err ... (ExpectingKeyword "let") ...
    run (keyword "let") "letters" == Err ... (ExpectingKeyword "let") ...

**Note:** Notice the third case there! `keyword` actually looks ahead one
character to make sure it is not a letter, number, or underscore. The goal is
to help with parsers like this:

    succeed identity
      |. keyword "let"
      |. spaces
      |= elmVar
      |. spaces
      |. symbol "="

The trouble is that `spaces` may chomp zero characters (to handle expressions
like `[1,2]` and `[ 1 , 2 ]`) and in this case, it would mean `letters` could
be parsed as `let ters` and then wonder where the equals sign is! Check out the
[`token`](#token) docs if you need to customize this!

keyword: String -> Parser.Parser ()
-}
keyword : String -> Elm.Expression
keyword keywordArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "keyword"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
        )
        [ Elm.string keywordArg ]


{-| Create a parser for variables. If we wanted to parse type variables in Elm,
we could try something like this:

    import Char
    import Parser exposing (..)
    import Set

    typeVar : Parser String
    typeVar =
      variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "let", "in", "case", "of" ]
        }

This is saying it _must_ start with a lower-case character. After that,
characters can be letters, numbers, or underscores. It is also saying that if
you run into any of these reserved names, it is definitely not a variable.

variable: 
    { start : Char.Char -> Bool
    , inner : Char.Char -> Bool
    , reserved : Set.Set String
    }
    -> Parser.Parser String
-}
variable :
    { start : Elm.Expression -> Elm.Expression
    , inner : Elm.Expression -> Elm.Expression
    , reserved : Elm.Expression
    }
    -> Elm.Expression
variable variableArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "variable"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "start", Type.function [ Type.char ] Type.bool )
                            , ( "inner", Type.function [ Type.char ] Type.bool )
                            , ( "reserved"
                              , Type.namedWith [ "Set" ] "Set" [ Type.string ]
                              )
                            ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.string ])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair
                "start"
                (Elm.functionReduced "variableUnpack" variableArg.start)
            , Tuple.pair
                "inner"
                (Elm.functionReduced "variableUnpack" variableArg.inner)
            , Tuple.pair "reserved" variableArg.reserved
            ]
        ]


{-| Check if you have reached the end of the string you are parsing.

    justAnInt : Parser Int
    justAnInt =
      succeed identity
        |= int
        |. end

    -- run justAnInt "90210" == Ok 90210
    -- run justAnInt "1 + 2" == Err ...
    -- run int       "1 + 2" == Ok 1

Parsers can succeed without parsing the whole string. Ending your parser
with `end` guarantees that you have successfully parsed the whole string.

end: Parser.Parser ()
-}
end : Elm.Expression
end =
    Elm.value
        { importFrom = [ "Parser" ]
        , name = "end"
        , annotation = Just (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
        }


{-| A parser that succeeds without chomping any characters.

    run (succeed 90210  ) "mississippi" == Ok 90210
    run (succeed 3.141  ) "mississippi" == Ok 3.141
    run (succeed ()     ) "mississippi" == Ok ()
    run (succeed Nothing) "mississippi" == Ok Nothing

Seems weird on its own, but it is very useful in combination with other
functions. The docs for [`(|=)`](#|=) and [`andThen`](#andThen) have some neat
examples.

succeed: a -> Parser.Parser a
-}
succeed : Elm.Expression -> Elm.Expression
succeed succeedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "succeed"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
        )
        [ succeedArg ]


{-| Helper to define recursive parsers. Say we want a parser for simple
boolean expressions:

    true
    false
    (true || false)
    (true || (true || false))

Notice that a boolean expression might contain *other* boolean expressions.
That means we will want to define our parser in terms of itself:

    type Boolean
      = MyTrue
      | MyFalse
      | MyOr Boolean Boolean

    boolean : Parser Boolean
    boolean =
      oneOf
        [ succeed MyTrue
            |. keyword "true"
        , succeed MyFalse
            |. keyword "false"
        , succeed MyOr
            |. symbol "("
            |. spaces
            |= lazy (\_ -> boolean)
            |. spaces
            |. symbol "||"
            |. spaces
            |= lazy (\_ -> boolean)
            |. spaces
            |. symbol ")"
        ]

**Notice that `boolean` uses `boolean` in its definition!** In Elm, you can
only define a value in terms of itself it is behind a function call. So
`lazy` helps us define these self-referential parsers. (`andThen` can be used
for this as well!)

lazy: (() -> Parser.Parser a) -> Parser.Parser a
-}
lazy : (Elm.Expression -> Elm.Expression) -> Elm.Expression
lazy lazyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "lazy"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.unit ]
                            (Type.namedWith
                                [ "Parser" ]
                                "Parser"
                                [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.functionReduced "lazyUnpack" lazyArg ]


{-| Parse one thing `andThen` parse another thing. This is useful when you want
to check on what you just parsed. For example, maybe you want U.S. zip codes
and `int` is not suitable because it does not allow leading zeros. You could
say:

    zipCode : Parser String
    zipCode =
      getChompedString (chompWhile Char.isDigit)
        |> andThen checkZipCode

    checkZipCode : String -> Parser String
    checkZipCode code =
      if String.length code == 5 then
        succeed code
      else
        problem "a U.S. zip code has exactly 5 digits"

First we chomp digits `andThen` we check if it is a valid U.S. zip code. We
`succeed` if it has exactly five digits and report a `problem` if not.

Check out [`examples/DoubleQuoteString.elm`](https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm)
for another example, this time using `andThen` to verify unicode code points.

**Note:** If you are using `andThen` recursively and blowing the stack, check
out the [`loop`](#loop) function to limit stack usage.

andThen: (a -> Parser.Parser b) -> Parser.Parser a -> Parser.Parser b
-}
andThen : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
andThen andThenArg andThenArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "andThen"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith
                                [ "Parser" ]
                                "Parser"
                                [ Type.var "b" ]
                            )
                        , Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "b" ])
                    )
            }
        )
        [ Elm.functionReduced "andThenUnpack" andThenArg, andThenArg0 ]


{-| Indicate that a parser has reached a dead end. "Everything was going fine
until I ran into this problem." Check out the [`andThen`](#andThen) docs to see
an example usage.

problem: String -> Parser.Parser a
-}
problem : String -> Elm.Expression
problem problemArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "problem"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.string problemArg ]


{-| If you are parsing JSON, the values can be strings, floats, booleans,
arrays, objects, or null. You need a way to pick `oneOf` them! Here is a
sample of what that code might look like:

    type Json
      = Number Float
      | Boolean Bool
      | Null

    json : Parser Json
    json =
      oneOf
        [ map Number float
        , map (\_ -> Boolean True) (keyword "true")
        , map (\_ -> Boolean False) (keyword "false")
        , map (\_ -> Null) keyword "null"
        ]

This parser will keep trying parsers until `oneOf` them starts chomping
characters. Once a path is chosen, it does not come back and try the others.

**Note:** I highly recommend reading [this document][semantics] to learn how
`oneOf` and `backtrackable` interact. It is subtle and important!

[semantics]: https://github.com/elm/parser/blob/master/semantics.md

oneOf: List (Parser.Parser a) -> Parser.Parser a
-}
oneOf : List Elm.Expression -> Elm.Expression
oneOf oneOfArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "oneOf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Parser" ]
                                "Parser"
                                [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.list oneOfArg ]


{-| Transform the result of a parser. Maybe you have a value that is
an integer or `null`:

    nullOrInt : Parser (Maybe Int)
    nullOrInt =
      oneOf
        [ map Just int
        , map (\_ -> Nothing) (keyword "null")
        ]

    -- run nullOrInt "0"    == Ok (Just 0)
    -- run nullOrInt "13"   == Ok (Just 13)
    -- run nullOrInt "null" == Ok Nothing
    -- run nullOrInt "zero" == Err ...

map: (a -> b) -> Parser.Parser a -> Parser.Parser b
-}
map : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
map mapArg mapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "b" ])
                    )
            }
        )
        [ Elm.functionReduced "mapUnpack" mapArg, mapArg0 ]


{-| It is quite tricky to use `backtrackable` well! It can be very useful, but
also can degrade performance and error message quality.

Read [this document](https://github.com/elm/parser/blob/master/semantics.md)
to learn how `oneOf`, `backtrackable`, and `commit` work and interact with
each other. It is subtle and important!

backtrackable: Parser.Parser a -> Parser.Parser a
-}
backtrackable : Elm.Expression -> Elm.Expression
backtrackable backtrackableArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "backtrackable"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
        )
        [ backtrackableArg ]


{-| `commit` is almost always paired with `backtrackable` in some way, and it
is tricky to use well.

Read [this document](https://github.com/elm/parser/blob/master/semantics.md)
to learn how `oneOf`, `backtrackable`, and `commit` work and interact with
each other. It is subtle and important!

commit: a -> Parser.Parser a
-}
commit : Elm.Expression -> Elm.Expression
commit commitArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "commit"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
        )
        [ commitArg ]


{-| Parse exactly the given string, without any regard to what comes next.

A potential pitfall when parsing keywords is getting tricked by variables that
start with a keyword, like `let` in `letters` or `import` in `important`. This
is especially likely if you have a whitespace parser that can consume zero
charcters. So the [`keyword`](#keyword) parser is defined with `token` and a
trick to peek ahead a bit:

    keyword : String -> Parser ()
    keyword kwd =
      succeed identity
        |. backtrackable (token kwd)
        |= oneOf
            [ map (\_ -> True) (backtrackable (chompIf isVarChar))
            , succeed False
            ]
        |> andThen (checkEnding kwd)

    checkEnding : String -> Bool -> Parser ()
    checkEnding kwd isBadEnding =
      if isBadEnding then
        problem ("expecting the `" ++ kwd ++ "` keyword")
      else
        commit ()

    isVarChar : Char -> Bool
    isVarChar char =
      Char.isAlphaNum char || char == '_'

This definition is specially designed so that (1) if you really see `let` you
commit to that path and (2) if you see `letters` instead you can backtrack and
try other options. If I had just put a `backtrackable` around the whole thing
you would not get (1) anymore.

token: String -> Parser.Parser ()
-}
token : String -> Elm.Expression
token tokenArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "token"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
        )
        [ Elm.string tokenArg ]


{-| Handle things like lists and records, but you can customize the details
however you need. Say you want to parse C-style code blocks:

    import Parser exposing (Parser, Trailing(..))

    block : Parser (List Stmt)
    block =
      Parser.sequence
        { start = "{"
        , separator = ";"
        , end = "}"
        , spaces = spaces
        , item = statement
        , trailing = Mandatory -- demand a trailing semi-colon
        }

    -- statement : Parser Stmt

**Note:** If you need something more custom, do not be afraid to check
out the implementation and customize it for your case. It is better to
get nice error messages with a lower-level implementation than to try
to hack high-level parsers to do things they are not made for.

sequence: 
    { start : String
    , separator : String
    , end : String
    , spaces : Parser.Parser ()
    , item : Parser.Parser a
    , trailing : Parser.Trailing
    }
    -> Parser.Parser (List a)
-}
sequence :
    { start : String
    , separator : String
    , end : String
    , spaces : Elm.Expression
    , item : Elm.Expression
    , trailing : Elm.Expression
    }
    -> Elm.Expression
sequence sequenceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "sequence"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "start", Type.string )
                            , ( "separator", Type.string )
                            , ( "end", Type.string )
                            , ( "spaces"
                              , Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                              )
                            , ( "item"
                              , Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                              )
                            , ( "trailing"
                              , Type.namedWith [ "Parser" ] "Trailing" []
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Parser" ]
                            "Parser"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "start" (Elm.string sequenceArg.start)
            , Tuple.pair "separator" (Elm.string sequenceArg.separator)
            , Tuple.pair "end" (Elm.string sequenceArg.end)
            , Tuple.pair "spaces" sequenceArg.spaces
            , Tuple.pair "item" sequenceArg.item
            , Tuple.pair "trailing" sequenceArg.trailing
            ]
        ]


{-| A parser that can loop indefinitely. This can be helpful when parsing
repeated structures, like a bunch of statements:

    statements : Parser (List Stmt)
    statements =
      loop [] statementsHelp

    statementsHelp : List Stmt -> Parser (Step (List Stmt) (List Stmt))
    statementsHelp revStmts =
      oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= statement
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]

    -- statement : Parser Stmt

Notice that the statements are tracked in reverse as we `Loop`, and we reorder
them only once we are `Done`. This is a very common pattern with `loop`!

Check out [`examples/DoubleQuoteString.elm`](https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm)
for another example.

**IMPORTANT NOTE:** Parsers like `succeed ()` and `chompWhile Char.isAlpha` can
succeed without consuming any characters. So in some cases you may want to use
[`getOffset`](#getOffset) to ensure that each step actually consumed characters.
Otherwise you could end up in an infinite loop!

**Note:** Anything you can write with `loop`, you can also write as a parser
that chomps some characters `andThen` calls itself with new arguments. The
problem with calling `andThen` recursively is that it grows the stack, so you
cannot do it indefinitely. So `loop` is important because enables tail-call
elimination, allowing you to parse however many repeats you want.

loop: state -> (state -> Parser.Parser (Parser.Step state a)) -> Parser.Parser a
-}
loop : Elm.Expression -> (Elm.Expression -> Elm.Expression) -> Elm.Expression
loop loopArg loopArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "loop"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "state"
                        , Type.function
                            [ Type.var "state" ]
                            (Type.namedWith
                                [ "Parser" ]
                                "Parser"
                                [ Type.namedWith
                                    [ "Parser" ]
                                    "Step"
                                    [ Type.var "state", Type.var "a" ]
                                ]
                            )
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
        )
        [ loopArg, Elm.functionReduced "loopUnpack" loopArg0 ]


{-| Parse zero or more `' '`, `'\n'`, and `'\r'` characters.

The implementation is pretty simple:

    spaces : Parser ()
    spaces =
      chompWhile (\c -> c == ' ' || c == '\n' || c == '\r')

So if you need something different (like tabs) just define an alternative with
the necessary tweaks! Check out [`lineComment`](#lineComment) and
[`multiComment`](#multiComment) for more complex situations.

spaces: Parser.Parser ()
-}
spaces : Elm.Expression
spaces =
    Elm.value
        { importFrom = [ "Parser" ]
        , name = "spaces"
        , annotation = Just (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
        }


{-| Parse single-line comments:

    elm : Parser ()
    elm =
      lineComment "--"

    js : Parser ()
    js =
      lineComment "//"

    python : Parser ()
    python =
      lineComment "#"

This parser is defined like this:

    lineComment : String -> Parser ()
    lineComment str =
      symbol str
        |. chompUntilEndOr "\n"

So it will consume the remainder of the line. If the file ends before you see
a newline, that is fine too.

lineComment: String -> Parser.Parser ()
-}
lineComment : String -> Elm.Expression
lineComment lineCommentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "lineComment"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
        )
        [ Elm.string lineCommentArg ]


{-| Parse multi-line comments. So if you wanted to parse Elm whitespace or
JS whitespace, you could say:

    elm : Parser ()
    elm =
      loop 0 <| ifProgress <|
        oneOf
          [ lineComment "--"
          , multiComment "{-" "-}" Nestable
          , spaces
          ]

    js : Parser ()
    js =
      loop 0 <| ifProgress <|
        oneOf
          [ lineComment "//"
          , multiComment "/*" "*/" NotNestable
          , chompWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')
          ]

    ifProgress : Parser a -> Int -> Parser (Step Int ())
    ifProgress parser offset =
      succeed identity
        |. parser
        |= getOffset
        |> map (\newOffset -> if offset == newOffset then Done () else Loop newOffset)

**Note:** The fact that `spaces` comes last in the definition of `elm` is very
important! It can succeed without consuming any characters, so if it were the
first option, it would always succeed and bypass the others! (Same is true of
`chompWhile` in `js`.) This possibility of success without consumption is also
why wee need the `ifProgress` helper. It detects if there is no more whitespace
to consume.

multiComment: String -> String -> Parser.Nestable -> Parser.Parser ()
-}
multiComment : String -> String -> Elm.Expression -> Elm.Expression
multiComment multiCommentArg multiCommentArg0 multiCommentArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "multiComment"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.string
                        , Type.namedWith [ "Parser" ] "Nestable" []
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
        )
        [ Elm.string multiCommentArg
        , Elm.string multiCommentArg0
        , multiCommentArg1
        ]


{-| Sometimes parsers like `int` or `variable` cannot do exactly what you
need. The "chomping" family of functions is meant for that case! Maybe you
need to parse [valid PHP variables][php] like `$x` and `$txt`:

    php : Parser String
    php =
      getChompedString <|
        succeed ()
          |. chompIf (\c -> c == '$')
          |. chompIf (\c -> Char.isAlpha c || c == '_')
          |. chompWhile (\c -> Char.isAlphaNum c || c == '_')

The idea is that you create a bunch of chompers that validate the underlying
characters. Then `getChompedString` extracts the underlying `String` efficiently.

**Note:** Maybe it is helpful to see how you can use [`getOffset`](#getOffset)
and [`getSource`](#getSource) to implement this function:

    getChompedString : Parser a -> Parser String
    getChompedString parser =
      succeed String.slice
        |= getOffset
        |. parser
        |= getOffset
        |= getSource

[php]: https://www.w3schools.com/php/php_variables.asp

getChompedString: Parser.Parser a -> Parser.Parser String
-}
getChompedString : Elm.Expression -> Elm.Expression
getChompedString getChompedStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "getChompedString"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.string ])
                    )
            }
        )
        [ getChompedStringArg ]


{-| Chomp one character if it passes the test.

    chompUpper : Parser ()
    chompUpper =
      chompIf Char.isUpper

So this can chomp a character like `T` and produces a `()` value.

chompIf: (Char.Char -> Bool) -> Parser.Parser ()
-}
chompIf : (Elm.Expression -> Elm.Expression) -> Elm.Expression
chompIf chompIfArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "chompIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.char ] Type.bool ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
        )
        [ Elm.functionReduced "chompIfUnpack" chompIfArg ]


{-| Chomp zero or more characters if they pass the test. This is commonly
useful for chomping whitespace or variable names:

    whitespace : Parser ()
    whitespace =
      chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

    elmVar : Parser String
    elmVar =
      getChompedString <|
        succeed ()
          |. chompIf Char.isLower
          |. chompWhile (\c -> Char.isAlphaNum c || c == '_')

**Note:** a `chompWhile` parser always succeeds! This can lead to tricky
situations, especially if you define your whitespace with it. In that case,
you could accidentally interpret `letx` as the keyword `let` followed by
"spaces" followed by the variable `x`. This is why the `keyword` and `number`
parsers peek ahead, making sure they are not followed by anything unexpected.

chompWhile: (Char.Char -> Bool) -> Parser.Parser ()
-}
chompWhile : (Elm.Expression -> Elm.Expression) -> Elm.Expression
chompWhile chompWhileArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "chompWhile"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.char ] Type.bool ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
        )
        [ Elm.functionReduced "chompWhileUnpack" chompWhileArg ]


{-| Chomp until you see a certain string. You could define C-style multi-line
comments like this:

    comment : Parser ()
    comment =
      symbol "/*"
        |. chompUntil "*/"

I recommend using [`multiComment`](#multiComment) for this particular scenario
though. It can be trickier than it looks!

chompUntil: String -> Parser.Parser ()
-}
chompUntil : String -> Elm.Expression
chompUntil chompUntilArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "chompUntil"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
        )
        [ Elm.string chompUntilArg ]


{-| Chomp until you see a certain string or until you run out of characters to
chomp! You could define single-line comments like this:

    elm : Parser ()
    elm =
      symbol "--"
        |. chompUntilEndOr "\n"

A file may end with a single-line comment, so the file can end before you see
a newline. Tricky!

I recommend just using [`lineComment`](#lineComment) for this particular
scenario.

chompUntilEndOr: String -> Parser.Parser ()
-}
chompUntilEndOr : String -> Elm.Expression
chompUntilEndOr chompUntilEndOrArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "chompUntilEndOr"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
        )
        [ Elm.string chompUntilEndOrArg ]


{-| This works just like [`getChompedString`](#getChompedString) but gives
a bit more flexibility. For example, maybe you want to parse Elm doc comments
and get (1) the full comment and (2) all of the names listed in the docs.

You could implement `mapChompedString` like this:

    mapChompedString : (String -> a -> b) -> Parser a -> Parser String
    mapChompedString func parser =
      succeed (\start value end src -> func (String.slice start end src) value)
        |= getOffset
        |= parser
        |= getOffset
        |= getSource

mapChompedString: (String -> a -> b) -> Parser.Parser a -> Parser.Parser b
-}
mapChompedString :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
mapChompedString mapChompedStringArg mapChompedStringArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "mapChompedString"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.string, Type.var "a" ]
                            (Type.var "b")
                        , Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "b" ])
                    )
            }
        )
        [ Elm.functionReduced
            "mapChompedStringUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (mapChompedStringArg functionReducedUnpack)
            )
        , mapChompedStringArg0
        ]


{-| Turn all the `DeadEnd` data into a string that is easier for people to
read.

**Note:** This is just a baseline of quality. It cannot do anything with colors.
It is not interactivite. It just turns the raw data into strings. I really hope
folks will check out the source code for some inspiration on how to turn errors
into `Html` with nice colors and interaction! The `Parser.Advanced` module lets
you work with context as well, which really unlocks another level of quality!
The "context" technique is how the Elm compiler can say "I think I am parsing a
list, so I was expecting a closing `]` here." Telling users what the parser
_thinks_ is happening can be really helpful!

deadEndsToString: List Parser.DeadEnd -> String
-}
deadEndsToString : List Elm.Expression -> Elm.Expression
deadEndsToString deadEndsToStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "deadEndsToString"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Parser" ] "DeadEnd" []) ]
                        Type.string
                    )
            }
        )
        [ Elm.list deadEndsToStringArg ]


{-| Some languages are indentation sensitive. Python cares about tabs. Elm
cares about spaces sometimes. `withIndent` and `getIndent` allow you to manage
"indentation state" yourself, however is necessary in your scenario.

withIndent: Int -> Parser.Parser a -> Parser.Parser a
-}
withIndent : Int -> Elm.Expression -> Elm.Expression
withIndent withIndentArg withIndentArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Parser" ]
            , name = "withIndent"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.int withIndentArg, withIndentArg0 ]


{-| When someone said `withIndent` earlier, what number did they put in there?

- `getIndent` results in `0`, the default value
- `withIndent 4 getIndent` results in `4`

So you are just asking about things you said earlier. These numbers do not leak
out of `withIndent`, so say we have:

    succeed Tuple.pair
      |= withIndent 4 getIndent
      |= getIndent

Assuming there are no `withIndent` above this, you would get `(4,0)` from this.

getIndent: Parser.Parser Int
-}
getIndent : Elm.Expression
getIndent =
    Elm.value
        { importFrom = [ "Parser" ]
        , name = "getIndent"
        , annotation = Just (Type.namedWith [ "Parser" ] "Parser" [ Type.int ])
        }


{-| Code editors treat code like a grid, with rows and columns. The start is
`row=1` and `col=1`. As you chomp characters, the `col` increments. When you
run into a `\n` character, the `row` increments and `col` goes back to `1`.

In the Elm compiler, I track the start and end position of every expression
like this:

    type alias Located a =
      { start : (Int, Int)
      , value : a
      , end : (Int, Int)
      }

    located : Parser a -> Parser (Located a)
    located parser =
      succeed Located
        |= getPosition
        |= parser
        |= getPosition

So if there is a problem during type inference, I use this saved position
information to underline the exact problem!

**Note:** Tabs count as one character, so if you are parsing something like
Python, I recommend sorting that out *after* parsing. So if I wanted the `^^^^`
underline like in Elm, I would find the `row` in the source code and do
something like this:

    makeUnderline : String -> Int -> Int -> String
    makeUnderline row minCol maxCol =
      String.toList row
        |> List.indexedMap (toUnderlineChar minCol maxCol)
        |> String.fromList

    toUnderlineChar : Int -> Int -> Int -> Char -> Char
    toUnderlineChar minCol maxCol col char =
      if minCol <= col && col <= maxCol then
        '^'
      else if char == '\t' then
        '\t'
      else
        ' '

So it would preserve any tabs from the source line. There are tons of other
ways to do this though. The point is just that you handle the tabs after
parsing but before anyone looks at the numbers in a context where tabs may
equal 2, 4, or 8.

getPosition: Parser.Parser ( Int, Int )
-}
getPosition : Elm.Expression
getPosition =
    Elm.value
        { importFrom = [ "Parser" ]
        , name = "getPosition"
        , annotation =
            Just
                (Type.namedWith
                    [ "Parser" ]
                    "Parser"
                    [ Type.tuple Type.int Type.int ]
                )
        }


{-| This is a more efficient version of `map Tuple.first getPosition`. Maybe
you just want to track the line number for some reason? This lets you do that.

See [`getPosition`](#getPosition) for an explanation of rows and columns.

getRow: Parser.Parser Int
-}
getRow : Elm.Expression
getRow =
    Elm.value
        { importFrom = [ "Parser" ]
        , name = "getRow"
        , annotation = Just (Type.namedWith [ "Parser" ] "Parser" [ Type.int ])
        }


{-| This is a more efficient version of `map Tuple.second getPosition`. This
can be useful in combination with [`withIndent`](#withIndent) and
[`getIndent`](#getIndent), like this:

    checkIndent : Parser ()
    checkIndent =
      succeed (\indent column -> indent <= column)
        |= getIndent
        |= getCol
        |> andThen checkIndentHelp

    checkIndentHelp : Bool -> Parser ()
    checkIndentHelp isIndented =
      if isIndented then
        succeed ()
      else
        problem "expecting more spaces"

So the `checkIndent` parser only succeeds when you are "deeper" than the
current indent level. You could use this to parse Elm-style `let` expressions.

getCol: Parser.Parser Int
-}
getCol : Elm.Expression
getCol =
    Elm.value
        { importFrom = [ "Parser" ]
        , name = "getCol"
        , annotation = Just (Type.namedWith [ "Parser" ] "Parser" [ Type.int ])
        }


{-| Editors think of code as a grid, but behind the scenes it is just a flat
array of UTF-16 characters. `getOffset` tells you your index in that flat
array. So if you chomp `"\n\n\n\n"` you are on row 5, column 1, and offset 4.

**Note:** JavaScript uses a somewhat odd version of UTF-16 strings, so a single
character may take two slots. So in JavaScript, `'abc'.length === 3` but
`'🙈🙉🙊'.length === 6`. Try it out! And since Elm runs in JavaScript, the offset
moves by those rules.

getOffset: Parser.Parser Int
-}
getOffset : Elm.Expression
getOffset =
    Elm.value
        { importFrom = [ "Parser" ]
        , name = "getOffset"
        , annotation = Just (Type.namedWith [ "Parser" ] "Parser" [ Type.int ])
        }


{-| Get the full string that is being parsed. You could use this to define
`getChompedString` or `mapChompedString` if you wanted:

    getChompedString : Parser a -> Parser String
    getChompedString parser =
      succeed String.slice
        |= getOffset
        |. parser
        |= getOffset
        |= getSource

getSource: Parser.Parser String
-}
getSource : Elm.Expression
getSource =
    Elm.value
        { importFrom = [ "Parser" ]
        , name = "getSource"
        , annotation =
            Just (Type.namedWith [ "Parser" ] "Parser" [ Type.string ])
        }


annotation_ :
    { parser : Type.Annotation -> Type.Annotation
    , trailing : Type.Annotation
    , step : Type.Annotation -> Type.Annotation -> Type.Annotation
    , nestable : Type.Annotation
    , deadEnd : Type.Annotation
    , problem : Type.Annotation
    }
annotation_ =
    { parser =
        \parserArg0 ->
            Type.alias
                moduleName_
                "Parser"
                [ parserArg0 ]
                (Type.namedWith
                    [ "Parser", "Advanced" ]
                    "Parser"
                    [ Type.namedWith [ "Basics" ] "Never" []
                    , Type.namedWith [ "Parser" ] "Problem" []
                    , Type.var "a"
                    ]
                )
    , trailing = Type.namedWith [ "Parser" ] "Trailing" []
    , step =
        \stepArg0 stepArg1 ->
            Type.namedWith [ "Parser" ] "Step" [ stepArg0, stepArg1 ]
    , nestable = Type.namedWith [ "Parser" ] "Nestable" []
    , deadEnd =
        Type.alias
            moduleName_
            "DeadEnd"
            []
            (Type.record
                [ ( "row", Type.int )
                , ( "col", Type.int )
                , ( "problem", Type.namedWith [ "Parser" ] "Problem" [] )
                ]
            )
    , problem = Type.namedWith [ "Parser" ] "Problem" []
    }


make_ :
    { forbidden : Elm.Expression
    , optional : Elm.Expression
    , mandatory : Elm.Expression
    , loop : Elm.Expression -> Elm.Expression
    , done : Elm.Expression -> Elm.Expression
    , notNestable : Elm.Expression
    , nestable : Elm.Expression
    , deadEnd :
        { row : Elm.Expression, col : Elm.Expression, problem : Elm.Expression }
        -> Elm.Expression
    , expecting : Elm.Expression -> Elm.Expression
    , expectingInt : Elm.Expression
    , expectingHex : Elm.Expression
    , expectingOctal : Elm.Expression
    , expectingBinary : Elm.Expression
    , expectingFloat : Elm.Expression
    , expectingNumber : Elm.Expression
    , expectingVariable : Elm.Expression
    , expectingSymbol : Elm.Expression -> Elm.Expression
    , expectingKeyword : Elm.Expression -> Elm.Expression
    , expectingEnd : Elm.Expression
    , unexpectedChar : Elm.Expression
    , problem : Elm.Expression -> Elm.Expression
    , badRepeat : Elm.Expression
    }
make_ =
    { forbidden =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "Forbidden"
            , annotation = Just (Type.namedWith [] "Trailing" [])
            }
    , optional =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "Optional"
            , annotation = Just (Type.namedWith [] "Trailing" [])
            }
    , mandatory =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "Mandatory"
            , annotation = Just (Type.namedWith [] "Trailing" [])
            }
    , loop =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "Loop"
                    , annotation =
                        Just
                            (Type.namedWith
                                []
                                "Step"
                                [ Type.var "state", Type.var "a" ]
                            )
                    }
                )
                [ ar0 ]
    , done =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "Done"
                    , annotation =
                        Just
                            (Type.namedWith
                                []
                                "Step"
                                [ Type.var "state", Type.var "a" ]
                            )
                    }
                )
                [ ar0 ]
    , notNestable =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "NotNestable"
            , annotation = Just (Type.namedWith [] "Nestable" [])
            }
    , nestable =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "Nestable"
            , annotation = Just (Type.namedWith [] "Nestable" [])
            }
    , deadEnd =
        \deadEnd_args ->
            Elm.withType
                (Type.alias
                    [ "Parser" ]
                    "DeadEnd"
                    []
                    (Type.record
                        [ ( "row", Type.int )
                        , ( "col", Type.int )
                        , ( "problem"
                          , Type.namedWith [ "Parser" ] "Problem" []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "row" deadEnd_args.row
                    , Tuple.pair "col" deadEnd_args.col
                    , Tuple.pair "problem" deadEnd_args.problem
                    ]
                )
    , expecting =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "Expecting"
                    , annotation = Just (Type.namedWith [] "Problem" [])
                    }
                )
                [ ar0 ]
    , expectingInt =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "ExpectingInt"
            , annotation = Just (Type.namedWith [] "Problem" [])
            }
    , expectingHex =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "ExpectingHex"
            , annotation = Just (Type.namedWith [] "Problem" [])
            }
    , expectingOctal =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "ExpectingOctal"
            , annotation = Just (Type.namedWith [] "Problem" [])
            }
    , expectingBinary =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "ExpectingBinary"
            , annotation = Just (Type.namedWith [] "Problem" [])
            }
    , expectingFloat =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "ExpectingFloat"
            , annotation = Just (Type.namedWith [] "Problem" [])
            }
    , expectingNumber =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "ExpectingNumber"
            , annotation = Just (Type.namedWith [] "Problem" [])
            }
    , expectingVariable =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "ExpectingVariable"
            , annotation = Just (Type.namedWith [] "Problem" [])
            }
    , expectingSymbol =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "ExpectingSymbol"
                    , annotation = Just (Type.namedWith [] "Problem" [])
                    }
                )
                [ ar0 ]
    , expectingKeyword =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "ExpectingKeyword"
                    , annotation = Just (Type.namedWith [] "Problem" [])
                    }
                )
                [ ar0 ]
    , expectingEnd =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "ExpectingEnd"
            , annotation = Just (Type.namedWith [] "Problem" [])
            }
    , unexpectedChar =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "UnexpectedChar"
            , annotation = Just (Type.namedWith [] "Problem" [])
            }
    , problem =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "Problem"
                    , annotation = Just (Type.namedWith [] "Problem" [])
                    }
                )
                [ ar0 ]
    , badRepeat =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "BadRepeat"
            , annotation = Just (Type.namedWith [] "Problem" [])
            }
    }


caseOf_ :
    { trailing :
        Elm.Expression
        -> { trailingTags_0_0
            | forbidden : Elm.Expression
            , optional : Elm.Expression
            , mandatory : Elm.Expression
        }
        -> Elm.Expression
    , step :
        Elm.Expression
        -> { stepTags_1_0
            | loop : Elm.Expression -> Elm.Expression
            , done : Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    , nestable :
        Elm.Expression
        -> { nestableTags_2_0
            | notNestable : Elm.Expression
            , nestable : Elm.Expression
        }
        -> Elm.Expression
    , problem :
        Elm.Expression
        -> { problemTags_3_0
            | expecting : Elm.Expression -> Elm.Expression
            , expectingInt : Elm.Expression
            , expectingHex : Elm.Expression
            , expectingOctal : Elm.Expression
            , expectingBinary : Elm.Expression
            , expectingFloat : Elm.Expression
            , expectingNumber : Elm.Expression
            , expectingVariable : Elm.Expression
            , expectingSymbol : Elm.Expression -> Elm.Expression
            , expectingKeyword : Elm.Expression -> Elm.Expression
            , expectingEnd : Elm.Expression
            , unexpectedChar : Elm.Expression
            , problem : Elm.Expression -> Elm.Expression
            , badRepeat : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { trailing =
        \trailingExpression trailingTags ->
            Elm.Case.custom
                trailingExpression
                (Type.namedWith [ "Parser" ] "Trailing" [])
                [ Elm.Case.branch0 "Forbidden" trailingTags.forbidden
                , Elm.Case.branch0 "Optional" trailingTags.optional
                , Elm.Case.branch0 "Mandatory" trailingTags.mandatory
                ]
    , step =
        \stepExpression stepTags ->
            Elm.Case.custom
                stepExpression
                (Type.namedWith
                    [ "Parser" ]
                    "Step"
                    [ Type.var "state", Type.var "a" ]
                )
                [ Elm.Case.branch1
                    "Loop"
                    ( "state", Type.var "state" )
                    stepTags.loop
                , Elm.Case.branch1 "Done" ( "a", Type.var "a" ) stepTags.done
                ]
    , nestable =
        \nestableExpression nestableTags ->
            Elm.Case.custom
                nestableExpression
                (Type.namedWith [ "Parser" ] "Nestable" [])
                [ Elm.Case.branch0 "NotNestable" nestableTags.notNestable
                , Elm.Case.branch0 "Nestable" nestableTags.nestable
                ]
    , problem =
        \problemExpression problemTags ->
            Elm.Case.custom
                problemExpression
                (Type.namedWith [ "Parser" ] "Problem" [])
                [ Elm.Case.branch1
                    "Expecting"
                    ( "string.String", Type.string )
                    problemTags.expecting
                , Elm.Case.branch0 "ExpectingInt" problemTags.expectingInt
                , Elm.Case.branch0 "ExpectingHex" problemTags.expectingHex
                , Elm.Case.branch0 "ExpectingOctal" problemTags.expectingOctal
                , Elm.Case.branch0 "ExpectingBinary" problemTags.expectingBinary
                , Elm.Case.branch0 "ExpectingFloat" problemTags.expectingFloat
                , Elm.Case.branch0 "ExpectingNumber" problemTags.expectingNumber
                , Elm.Case.branch0
                    "ExpectingVariable"
                    problemTags.expectingVariable
                , Elm.Case.branch1
                    "ExpectingSymbol"
                    ( "string.String", Type.string )
                    problemTags.expectingSymbol
                , Elm.Case.branch1
                    "ExpectingKeyword"
                    ( "string.String", Type.string )
                    problemTags.expectingKeyword
                , Elm.Case.branch0 "ExpectingEnd" problemTags.expectingEnd
                , Elm.Case.branch0 "UnexpectedChar" problemTags.unexpectedChar
                , Elm.Case.branch1
                    "Problem"
                    ( "string.String", Type.string )
                    problemTags.problem
                , Elm.Case.branch0 "BadRepeat" problemTags.badRepeat
                ]
    }


call_ :
    { run : Elm.Expression -> Elm.Expression -> Elm.Expression
    , number : Elm.Expression -> Elm.Expression
    , symbol : Elm.Expression -> Elm.Expression
    , keyword : Elm.Expression -> Elm.Expression
    , variable : Elm.Expression -> Elm.Expression
    , succeed : Elm.Expression -> Elm.Expression
    , lazy : Elm.Expression -> Elm.Expression
    , andThen : Elm.Expression -> Elm.Expression -> Elm.Expression
    , problem : Elm.Expression -> Elm.Expression
    , oneOf : Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression
    , backtrackable : Elm.Expression -> Elm.Expression
    , commit : Elm.Expression -> Elm.Expression
    , token : Elm.Expression -> Elm.Expression
    , sequence : Elm.Expression -> Elm.Expression
    , loop : Elm.Expression -> Elm.Expression -> Elm.Expression
    , lineComment : Elm.Expression -> Elm.Expression
    , multiComment :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , getChompedString : Elm.Expression -> Elm.Expression
    , chompIf : Elm.Expression -> Elm.Expression
    , chompWhile : Elm.Expression -> Elm.Expression
    , chompUntil : Elm.Expression -> Elm.Expression
    , chompUntilEndOr : Elm.Expression -> Elm.Expression
    , mapChompedString : Elm.Expression -> Elm.Expression -> Elm.Expression
    , deadEndsToString : Elm.Expression -> Elm.Expression
    , withIndent : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { run =
        \runArg runArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "run"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                , Type.string
                                ]
                                (Type.namedWith
                                    [ "Result" ]
                                    "Result"
                                    [ Type.list
                                        (Type.namedWith
                                            [ "Parser" ]
                                            "DeadEnd"
                                            []
                                        )
                                    , Type.var "a"
                                    ]
                                )
                            )
                    }
                )
                [ runArg, runArg0 ]
    , number =
        \numberArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "number"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "int"
                                      , Type.maybe
                                            (Type.function
                                                [ Type.int ]
                                                (Type.var "a")
                                            )
                                      )
                                    , ( "hex"
                                      , Type.maybe
                                            (Type.function
                                                [ Type.int ]
                                                (Type.var "a")
                                            )
                                      )
                                    , ( "octal"
                                      , Type.maybe
                                            (Type.function
                                                [ Type.int ]
                                                (Type.var "a")
                                            )
                                      )
                                    , ( "binary"
                                      , Type.maybe
                                            (Type.function
                                                [ Type.int ]
                                                (Type.var "a")
                                            )
                                      )
                                    , ( "float"
                                      , Type.maybe
                                            (Type.function
                                                [ Type.float ]
                                                (Type.var "a")
                                            )
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ numberArg ]
    , symbol =
        \symbolArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "symbol"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                                )
                            )
                    }
                )
                [ symbolArg ]
    , keyword =
        \keywordArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "keyword"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                                )
                            )
                    }
                )
                [ keywordArg ]
    , variable =
        \variableArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "variable"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "start"
                                      , Type.function [ Type.char ] Type.bool
                                      )
                                    , ( "inner"
                                      , Type.function [ Type.char ] Type.bool
                                      )
                                    , ( "reserved"
                                      , Type.namedWith
                                            [ "Set" ]
                                            "Set"
                                            [ Type.string ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.string ]
                                )
                            )
                    }
                )
                [ variableArg ]
    , succeed =
        \succeedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "succeed"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "a" ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ succeedArg ]
    , lazy =
        \lazyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "lazy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.unit ]
                                    (Type.namedWith
                                        [ "Parser" ]
                                        "Parser"
                                        [ Type.var "a" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ lazyArg ]
    , andThen =
        \andThenArg andThenArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "andThen"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith
                                        [ "Parser" ]
                                        "Parser"
                                        [ Type.var "b" ]
                                    )
                                , Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ andThenArg, andThenArg0 ]
    , problem =
        \problemArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "problem"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ problemArg ]
    , oneOf =
        \oneOfArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "oneOf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Parser" ]
                                        "Parser"
                                        [ Type.var "a" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ oneOfArg ]
    , map =
        \mapArg mapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "map"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] (Type.var "b")
                                , Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ mapArg, mapArg0 ]
    , backtrackable =
        \backtrackableArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "backtrackable"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ backtrackableArg ]
    , commit =
        \commitArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "commit"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "a" ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ commitArg ]
    , token =
        \tokenArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "token"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                                )
                            )
                    }
                )
                [ tokenArg ]
    , sequence =
        \sequenceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "sequence"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "start", Type.string )
                                    , ( "separator", Type.string )
                                    , ( "end", Type.string )
                                    , ( "spaces"
                                      , Type.namedWith
                                            [ "Parser" ]
                                            "Parser"
                                            [ Type.unit ]
                                      )
                                    , ( "item"
                                      , Type.namedWith
                                            [ "Parser" ]
                                            "Parser"
                                            [ Type.var "a" ]
                                      )
                                    , ( "trailing"
                                      , Type.namedWith
                                            [ "Parser" ]
                                            "Trailing"
                                            []
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.list (Type.var "a") ]
                                )
                            )
                    }
                )
                [ sequenceArg ]
    , loop =
        \loopArg loopArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "loop"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "state"
                                , Type.function
                                    [ Type.var "state" ]
                                    (Type.namedWith
                                        [ "Parser" ]
                                        "Parser"
                                        [ Type.namedWith
                                            [ "Parser" ]
                                            "Step"
                                            [ Type.var "state", Type.var "a" ]
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ loopArg, loopArg0 ]
    , lineComment =
        \lineCommentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "lineComment"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                                )
                            )
                    }
                )
                [ lineCommentArg ]
    , multiComment =
        \multiCommentArg multiCommentArg0 multiCommentArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "multiComment"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.string
                                , Type.namedWith [ "Parser" ] "Nestable" []
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                                )
                            )
                    }
                )
                [ multiCommentArg, multiCommentArg0, multiCommentArg1 ]
    , getChompedString =
        \getChompedStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "getChompedString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.string ]
                                )
                            )
                    }
                )
                [ getChompedStringArg ]
    , chompIf =
        \chompIfArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "chompIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.char ] Type.bool ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                                )
                            )
                    }
                )
                [ chompIfArg ]
    , chompWhile =
        \chompWhileArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "chompWhile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.char ] Type.bool ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                                )
                            )
                    }
                )
                [ chompWhileArg ]
    , chompUntil =
        \chompUntilArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "chompUntil"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                                )
                            )
                    }
                )
                [ chompUntilArg ]
    , chompUntilEndOr =
        \chompUntilEndOrArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "chompUntilEndOr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                                )
                            )
                    }
                )
                [ chompUntilEndOrArg ]
    , mapChompedString =
        \mapChompedStringArg mapChompedStringArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "mapChompedString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.string, Type.var "a" ]
                                    (Type.var "b")
                                , Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ mapChompedStringArg, mapChompedStringArg0 ]
    , deadEndsToString =
        \deadEndsToStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "deadEndsToString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Parser" ] "DeadEnd" [])
                                ]
                                Type.string
                            )
                    }
                )
                [ deadEndsToStringArg ]
    , withIndent =
        \withIndentArg withIndentArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Parser" ]
                    , name = "withIndent"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ withIndentArg, withIndentArg0 ]
    }


values_ :
    { run : Elm.Expression
    , int : Elm.Expression
    , float : Elm.Expression
    , number : Elm.Expression
    , symbol : Elm.Expression
    , keyword : Elm.Expression
    , variable : Elm.Expression
    , end : Elm.Expression
    , succeed : Elm.Expression
    , lazy : Elm.Expression
    , andThen : Elm.Expression
    , problem : Elm.Expression
    , oneOf : Elm.Expression
    , map : Elm.Expression
    , backtrackable : Elm.Expression
    , commit : Elm.Expression
    , token : Elm.Expression
    , sequence : Elm.Expression
    , loop : Elm.Expression
    , spaces : Elm.Expression
    , lineComment : Elm.Expression
    , multiComment : Elm.Expression
    , getChompedString : Elm.Expression
    , chompIf : Elm.Expression
    , chompWhile : Elm.Expression
    , chompUntil : Elm.Expression
    , chompUntilEndOr : Elm.Expression
    , mapChompedString : Elm.Expression
    , deadEndsToString : Elm.Expression
    , withIndent : Elm.Expression
    , getIndent : Elm.Expression
    , getPosition : Elm.Expression
    , getRow : Elm.Expression
    , getCol : Elm.Expression
    , getOffset : Elm.Expression
    , getSource : Elm.Expression
    }
values_ =
    { run =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "run"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        , Type.string
                        ]
                        (Type.namedWith
                            [ "Result" ]
                            "Result"
                            [ Type.list
                                (Type.namedWith [ "Parser" ] "DeadEnd" [])
                            , Type.var "a"
                            ]
                        )
                    )
            }
    , int =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "int"
            , annotation =
                Just (Type.namedWith [ "Parser" ] "Parser" [ Type.int ])
            }
    , float =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "float"
            , annotation =
                Just (Type.namedWith [ "Parser" ] "Parser" [ Type.float ])
            }
    , number =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "number"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "int"
                              , Type.maybe
                                    (Type.function [ Type.int ] (Type.var "a"))
                              )
                            , ( "hex"
                              , Type.maybe
                                    (Type.function [ Type.int ] (Type.var "a"))
                              )
                            , ( "octal"
                              , Type.maybe
                                    (Type.function [ Type.int ] (Type.var "a"))
                              )
                            , ( "binary"
                              , Type.maybe
                                    (Type.function [ Type.int ] (Type.var "a"))
                              )
                            , ( "float"
                              , Type.maybe
                                    (Type.function [ Type.float ] (Type.var "a")
                                    )
                              )
                            ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
    , symbol =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "symbol"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
    , keyword =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "keyword"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
    , variable =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "variable"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "start", Type.function [ Type.char ] Type.bool )
                            , ( "inner", Type.function [ Type.char ] Type.bool )
                            , ( "reserved"
                              , Type.namedWith [ "Set" ] "Set" [ Type.string ]
                              )
                            ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.string ])
                    )
            }
    , end =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "end"
            , annotation =
                Just (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
            }
    , succeed =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "succeed"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
    , lazy =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "lazy"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.unit ]
                            (Type.namedWith
                                [ "Parser" ]
                                "Parser"
                                [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
    , andThen =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "andThen"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith
                                [ "Parser" ]
                                "Parser"
                                [ Type.var "b" ]
                            )
                        , Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "b" ])
                    )
            }
    , problem =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "problem"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
    , oneOf =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "oneOf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Parser" ]
                                "Parser"
                                [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "b" ])
                    )
            }
    , backtrackable =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "backtrackable"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
    , commit =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "commit"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
    , token =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "token"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
    , sequence =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "sequence"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "start", Type.string )
                            , ( "separator", Type.string )
                            , ( "end", Type.string )
                            , ( "spaces"
                              , Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.unit ]
                              )
                            , ( "item"
                              , Type.namedWith
                                    [ "Parser" ]
                                    "Parser"
                                    [ Type.var "a" ]
                              )
                            , ( "trailing"
                              , Type.namedWith [ "Parser" ] "Trailing" []
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Parser" ]
                            "Parser"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
    , loop =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "loop"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "state"
                        , Type.function
                            [ Type.var "state" ]
                            (Type.namedWith
                                [ "Parser" ]
                                "Parser"
                                [ Type.namedWith
                                    [ "Parser" ]
                                    "Step"
                                    [ Type.var "state", Type.var "a" ]
                                ]
                            )
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
    , spaces =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "spaces"
            , annotation =
                Just (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
            }
    , lineComment =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "lineComment"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
    , multiComment =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "multiComment"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.string
                        , Type.namedWith [ "Parser" ] "Nestable" []
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
    , getChompedString =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "getChompedString"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.string ])
                    )
            }
    , chompIf =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "chompIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.char ] Type.bool ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
    , chompWhile =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "chompWhile"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.char ] Type.bool ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
    , chompUntil =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "chompUntil"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
    , chompUntilEndOr =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "chompUntilEndOr"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.unit ])
                    )
            }
    , mapChompedString =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "mapChompedString"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.string, Type.var "a" ]
                            (Type.var "b")
                        , Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "b" ])
                    )
            }
    , deadEndsToString =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "deadEndsToString"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Parser" ] "DeadEnd" []) ]
                        Type.string
                    )
            }
    , withIndent =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "withIndent"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Parser" ] "Parser" [ Type.var "a" ])
                    )
            }
    , getIndent =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "getIndent"
            , annotation =
                Just (Type.namedWith [ "Parser" ] "Parser" [ Type.int ])
            }
    , getPosition =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "getPosition"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Parser" ]
                        "Parser"
                        [ Type.tuple Type.int Type.int ]
                    )
            }
    , getRow =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "getRow"
            , annotation =
                Just (Type.namedWith [ "Parser" ] "Parser" [ Type.int ])
            }
    , getCol =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "getCol"
            , annotation =
                Just (Type.namedWith [ "Parser" ] "Parser" [ Type.int ])
            }
    , getOffset =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "getOffset"
            , annotation =
                Just (Type.namedWith [ "Parser" ] "Parser" [ Type.int ])
            }
    , getSource =
        Elm.value
            { importFrom = [ "Parser" ]
            , name = "getSource"
            , annotation =
                Just (Type.namedWith [ "Parser" ] "Parser" [ Type.string ])
            }
    }


