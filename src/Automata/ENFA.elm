module Automata.ENFA exposing (..)

import Automata.NFA as FSA exposing (NFA)
import List.Extra exposing (unique)
import Maybe.Extra


{-| ε-NFA: finite state automaton with empty symbol ε (epsilon) represented by `Maybe.Nothing`.
-}
type alias ENFA state symbol =
    NFA state (Maybe symbol)


{-| Convert FSA to εFSA with no actual εs.
-}
fromNFA : NFA state symbol -> ENFA state symbol
fromNFA nfa =
    { start = nfa.start
    , ends = nfa.ends
    , transitions = nfa.transitions |> List.map (\t -> { from = t.from, symbol = Just t.symbol, to = t.to })
    }


emptyPaths : ENFA state symbol -> List (FSA.Path state (Maybe symbol))
emptyPaths enfa =
    let
        aux : List (FSA.Path state (Maybe symbol)) -> List state -> List state -> List (FSA.Path state (Maybe symbol))
        aux acc newStates seenStates =
            case newStates of
                first :: rest ->
                    if List.member first seenStates then
                        acc

                    else
                        let
                            nextStates =
                                FSA.step Nothing { enfa | start = first }
                                    |> List.filter (\newState -> not (List.member newState seenStates))

                            nextPaths =
                                nextStates
                                    |> List.concatMap (\newState -> acc |> List.map (\p -> ( Nothing, newState ) :: p))
                        in
                        aux (acc ++ nextPaths) (rest ++ nextStates) (first :: seenStates)

                [] ->
                    acc
    in
    aux [ [] ] [ enfa.start ] [] |> unique


emptyStep : ENFA state symbol -> List state
emptyStep fsa =
    emptyPaths fsa |> List.map (List.Extra.last >> Maybe.map Tuple.second >> Maybe.withDefault fsa.start)


stepPaths : symbol -> ENFA state symbol -> List (FSA.Path state (Maybe symbol))
stepPaths input enfa =
    emptyPaths enfa
        |> List.concatMap
            (\path ->
                let
                    lastState =
                        path |> List.Extra.last |> Maybe.map Tuple.second |> Maybe.withDefault enfa.start

                    nextStates =
                        FSA.step (Just input) { enfa | start = lastState }
                in
                nextStates |> List.map (\nextState -> path ++ [ ( Just input, nextState ) ])
            )


step : symbol -> ENFA state symbol -> List state
step input efsa =
    emptyStep efsa |> List.concatMap (\state -> FSA.step (Just input) { efsa | start = state })


check : List symbol -> ENFA state symbol -> Bool
check inputs efsa =
    case inputs of
        first :: rest ->
            step first efsa |> List.any (\newState -> check rest { efsa | start = newState })

        [] ->
            FSA.ended efsa


recurse :
    (List symbol -> ENFA state symbol -> List (FSA.Path state (Maybe symbol)))
    -> symbol
    -> List symbol
    -> ENFA state symbol
    -> List (FSA.Path state (Maybe symbol))
recurse pathsFn first rest efa =
    stepPaths first efa
        |> List.concatMap
            (\path ->
                let
                    lastState =
                        path |> List.Extra.last |> Maybe.map Tuple.second |> Maybe.withDefault efa.start

                    nextPaths =
                        pathsFn rest { efa | start = lastState }
                in
                nextPaths |> List.map (\nextPath -> path ++ nextPath)
            )


paths : List symbol -> ENFA state symbol -> List (FSA.Path state (Maybe symbol))
paths inputs efa =
    case inputs of
        first :: rest ->
            recurse paths first rest efa

        [] ->
            if FSA.ended efa then
                [ [] ]

            else
                []


allPaths : List symbol -> ENFA state symbol -> List (FSA.Path state (Maybe symbol))
allPaths inputs efa =
    case inputs of
        first :: rest ->
            recurse allPaths first rest efa

        [] ->
            [ [] ]


generate : Int -> ENFA state symbol -> List (List symbol)
generate n efsa =
    FSA.generate n efsa |> List.map Maybe.Extra.values


isEpsilonFree : ENFA state symbol -> Bool
isEpsilonFree efsa =
    efsa.transitions |> List.filter (.symbol >> (==) Nothing) |> List.isEmpty


toNFA : ENFA state symbol -> NFA state symbol
toNFA efsa =
    let
        deltas : List ( state, List state )
        deltas =
            efsa |> FSA.states |> List.map (\s -> ( s, emptyStep { efsa | start = s } ))

        transitionsFrom : List state -> List (FSA.Transition state (Maybe symbol))
        transitionsFrom origins =
            efsa.transitions |> List.filter (\t -> List.member t.from origins && not (t.symbol == Nothing))

        transitions : List (FSA.Transition state symbol)
        transitions =
            deltas
                |> List.concatMap
                    (\( from, tos ) ->
                        transitionsFrom tos
                            |> List.map (\t -> t.symbol |> Maybe.map (\sym -> { from = from, to = t.to, symbol = sym }))
                            |> Maybe.Extra.values
                    )
                |> unique
    in
    NFA efsa.start efsa.ends transitions
