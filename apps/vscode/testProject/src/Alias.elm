module Alias exposing (Alias, test)

import Imported


type alias Alias inner =
    Imported.MyType inner


test : val -> Alias val
test =
    Imported.MyValue
