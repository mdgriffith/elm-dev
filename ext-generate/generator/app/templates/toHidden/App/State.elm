module App.State exposing
    ( Cache, init, get
    , insert
    , remove, purge
    , values
    , Limit, initLimit
    , addToLimit, removeFromLimit
    )

{-|

@docs Cache, init, get

@docs insert

@docs remove, purge

@docs values

@docs Limit, initLimit
@docs addToLimit, removeFromLimit

-}

import Dict
import Set


type Cache state
    = Cache (Dict.Dict String state)


{-| -}
init : Cache state
init =
    Cache Dict.empty


{-| -}
get : String -> Cache state -> Maybe state
get key (Cache cache) =
    Dict.get key cache


{-| -}
insert : String -> state -> Cache state -> Cache state
insert key newState (Cache cache) =
    Cache (Dict.insert key newState cache)


{-| -}
remove : String -> Cache state -> Cache state
remove key (Cache cache) =
    Cache (Dict.remove key cache)


{-| -}
purge : List String -> Cache state -> Cache state
purge keys (Cache cache) =
    Cache (List.foldl Dict.remove cache keys)


{-| -}
values : Cache state -> List state
values (Cache cache) =
    Dict.values cache


{-| A data structure for keeping tracking of the number of instances per page-group we have.
-}
type Limit
    = Limit (Dict.Dict String (List String))


{-| -}
initLimit : Limit
initLimit =
    Limit Dict.empty


{-| -}
removeFromLimit :
    { groupId : String
    , instanceId : String
    }
    -> Limit
    -> Limit
removeFromLimit { groupId, instanceId } (Limit groups) =
    let
        group =
            Dict.get groupId groups
                |> Maybe.withDefault []

        newGroup =
            List.filter (\id -> id /= instanceId) group
    in
    Limit (Dict.insert groupId newGroup groups)


{-|

    - group is the page-group name.
    - instance is the id of the instance.
    - max is the number of items allowed, ignoring everything in the `keep` set.
    - keep is a set of ids that should not be removed.

-}
addToLimit :
    { groupId : String
    , instanceId : String
    , max : Int
    , keep : Set.Set String
    }
    -> Limit
    ->
        { limit : Limit
        , removedIds : List String
        }
addToLimit { groupId, instanceId, max, keep } (Limit groups) =
    let
        group =
            Dict.get groupId groups
                |> Maybe.withDefault []

        ( removableIds, necessaryInstanceIds ) =
            List.partition (\id -> id /= instanceId && not (Set.member id keep)) group

        removedIds =
            -- Remove everything beyond the limit.
            List.drop max removableIds

        cachedIds =
            List.take max removableIds
    in
    { limit =
        groups
            |> Dict.insert groupId
                (instanceId :: necessaryInstanceIds ++ cachedIds)
            |> Limit
    , removedIds = removedIds
    }
