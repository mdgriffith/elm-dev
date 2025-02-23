module Effect.Random exposing (generate)

{-|

@docs generate

-}

import Effect
import Random


{-| Run a random generator to produce a value.
-}
generate : (item -> msg) -> Random.Generator item -> Effect.Effect msg
generate fn generator =
    Effect.Generate (Random.map fn generator)
