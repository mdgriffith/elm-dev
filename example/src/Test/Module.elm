module Test.Module exposing (InvalidReason(..), Reason(..), myOtherFunction)


type Reason
    = Custom
    | Equality String String
    | Comparison String String
      -- Expected, actual, (index of problem, expected element, actual element)
    | ListDiff (List String) (List String)
      {- I don't think we need to show the diff twice with + and - reversed. Just show it after the main vertical bar.
         "Extra" and "missing" are relative to the actual value.
      -}
    | CollectionDiff
        { expected : String
        , actual : String
        , extra : List String
        , missing : List String
        }
    | TODO
    | Invalid InvalidReason


type InvalidReason
    = EmptyList
    | NonpositiveFuzzCount
    | InvalidFuzzer
    | BadDescription
    | DuplicatedName


myOtherFunction : String -> String
myOtherFunction incoming =
    incoming ++ ", the Best"
