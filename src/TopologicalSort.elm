module TopologicalSort exposing (SortError(..), sort)

{-| Topological sort of a directed graph.

Special version for sorting let declarations in Elm:

  - every declaration defines one or more variables
  - every declaration references zero or more free variables
  - cycles are allowed if they consist only of functions

-}

import FastDict as Dict exposing (Dict)
import Set exposing (Set)


type SortError
    = IllegalCycle
    | InternalError


type alias State a c =
    { sorted : List a
    , temporary : Set c
    , permanent : Set c
    , path : List c
    , error : Maybe SortError
    }


sort :
    { id : a -> comparable1
    , defVars : a -> Set comparable2
    , refVars : a -> Set comparable2
    , cycleAllowed : a -> Bool
    }
    -> (List a -> Result SortError (List a))
sort config items =
    let
        graph :
            { goalIds : List comparable1
            , cycleIds : Set comparable1
            , id2item : Dict comparable1 a
            , id2refs : Dict comparable1 (Set comparable1)
            }
        graph =
            items
                |> List.foldl
                    (\item acc ->
                        let
                            id : comparable1
                            id =
                                config.id item

                            defVars : Set comparable2
                            defVars =
                                config.defVars item

                            refVars : Set comparable2
                            refVars =
                                config.refVars item
                        in
                        { var2id =
                            Set.foldl
                                (\defVar acc2 ->
                                    Dict.insert defVar id acc2
                                )
                                acc.var2id
                                defVars

                        --
                        , goalIds = id :: acc.goalIds

                        --
                        , cycleIds =
                            if config.cycleAllowed item then
                                Set.insert id acc.cycleIds

                            else
                                acc.cycleIds

                        --
                        , id2item =
                            Dict.insert id item acc.id2item

                        --
                        , id2refs =
                            case Dict.get id acc.id2refs of
                                Nothing ->
                                    Dict.insert id refVars acc.id2refs

                                Just refs ->
                                    Dict.insert id (Set.union refs refVars) acc.id2refs
                        }
                    )
                    { var2id = Dict.empty
                    , goalIds = []
                    , cycleIds = Set.empty
                    , id2item = Dict.empty
                    , id2refs = Dict.empty
                    }
                |> (\{ var2id, goalIds, cycleIds, id2item, id2refs } ->
                        { goalIds = goalIds
                        , cycleIds = cycleIds
                        , id2item = id2item
                        , id2refs =
                            Dict.map
                                (\_ ->
                                    Set.foldl
                                        (\ref acc ->
                                            case Dict.get ref var2id of
                                                Nothing ->
                                                    acc

                                                Just id ->
                                                    Set.insert id acc
                                        )
                                        Set.empty
                                )
                                id2refs
                        }
                   )

        checkCycle : comparable1 -> List comparable1 -> Maybe SortError
        checkCycle start path =
            case path of
                h :: t ->
                    if Set.member h graph.cycleIds then
                        if h == start then
                            Nothing

                        else
                            checkCycle start t

                    else
                        Just IllegalCycle

                [] ->
                    Just InternalError

        visit : comparable1 -> State a comparable1 -> State a comparable1
        visit node state =
            case state.error of
                Just _ ->
                    state

                Nothing ->
                    if Set.member node state.permanent then
                        state

                    else if Set.member node state.temporary then
                        { state | error = checkCycle node state.path }

                    else
                        let
                            newState : State a comparable1
                            newState =
                                Set.foldl visit
                                    { state
                                        | temporary = Set.insert node state.temporary
                                        , path = node :: state.path
                                    }
                                    (Dict.get node graph.id2refs |> Maybe.withDefault Set.empty)
                        in
                        { state
                            | sorted =
                                case Dict.get node graph.id2item of
                                    Nothing ->
                                        newState.sorted

                                    Just item ->
                                        item :: newState.sorted
                            , permanent = Set.insert node newState.permanent
                            , error = newState.error
                        }

        result : State a comparable1
        result =
            List.foldl visit
                { sorted = []
                , temporary = Set.empty
                , permanent = Set.empty
                , path = []
                , error = Nothing
                }
                graph.goalIds
    in
    case result.error of
        Just err ->
            Err err

        Nothing ->
            Ok result.sorted
