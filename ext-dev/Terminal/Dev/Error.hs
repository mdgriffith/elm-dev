module Terminal.Dev.Error (Error(..), toString) where






data Error = CouldNotFindRoot  | CouldNotFindModule | ValueProvidedIsntQualified | ValueProvidedIsEmpty


toString :: Error -> String
toString err =
  case err of
    CouldNotFindRoot ->
      "Could not find elm.json file in this directory or any parent directories."

    CouldNotFindModule ->
      "Could not find module."

    ValueProvidedIsntQualified ->
      "Value provided isn't qualified."

    ValueProvidedIsEmpty ->
      "Value provided is empty."