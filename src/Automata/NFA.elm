module Automata.NFA exposing (..)

import Basics.Extra exposing (flip)
import List.Assoc
import List.Extra exposing (unique)
import Maybe.Extra exposing (isJust)


type alias Transition state symbol =
    { from : state, symbol : symbol, to : state }


{-| Finite State Automaton.
-}
type alias NFA state symbol =
    { start : state
    , ends : List state
    , transitions : List (Transition state symbol)
    }


type alias Step state symbol =
    ( symbol, state )


type alias Path state symbol =
    List (Step state symbol)


{-| List of states
-}
states : NFA state symbol -> List state
states nfa =
    nfa.start :: nfa.ends ++ (nfa.transitions |> List.concatMap (\{ from, to } -> [ from, to ])) |> unique


{-| List of symbols
-}
symbols : NFA state symbol -> List symbol
symbols nfa =
    nfa.transitions |> List.map .symbol |> unique


{-| Wether the start state is an end state.
-}
ended : NFA state symbol -> Bool
ended nfa =
    List.member nfa.start nfa.ends


{-| States reachable by taking one step and consuming one input symbol.
-}
step : symbol -> NFA state symbol -> List state
step input nfa =
    nfa.transitions |> List.filter (\t -> t.from == nfa.start && t.symbol == input) |> List.map .to


recurse :
    (List symbol -> NFA state symbol -> List (Path state symbol))
    -> symbol
    -> List symbol
    -> NFA state symbol
    -> List (Path state symbol)
recurse pathsFn first rest nfa =
    step first nfa
        |> List.concatMap (\newState -> pathsFn rest nfa |> List.map (\p -> ( first, newState ) :: p))


{-| All possible paths consuming the input.
-}
paths : List symbol -> NFA state symbol -> List (Path state symbol)
paths inputs nfa =
    case inputs of
        first :: rest ->
            recurse paths first rest nfa

        [] ->
            if ended nfa then
                [ [] ]

            else
                []


allPaths : List symbol -> NFA state symbol -> List (Path state symbol)
allPaths inputs nfa =
    case inputs of
        first :: rest ->
            step first nfa
                |> List.concatMap (\newState -> paths rest nfa |> List.map (\p -> ( first, newState ) :: p))

        [] ->
            [ [] ]


{-| Wether we reach an end state after consuming the input.
-}
check : List symbol -> NFA state symbol -> Bool
check inputs nfa =
    case inputs of
        first :: rest ->
            step first nfa |> List.any (\newState -> check rest { nfa | start = newState })

        [] ->
            ended nfa


{-| Generate valid inputs with a given maximum length.
-}
generate : Int -> NFA state symbol -> List (List symbol)
generate n nfa =
    List.concat
        [ if ended nfa then
            [ [] ]

          else
            []
        , if n > 0 then
            nfa.transitions
                |> List.filter (\e -> e.from == nfa.start)
                |> List.concatMap
                    (\{ symbol, to } ->
                        generate (n - 1) { nfa | start = to }
                            |> List.Extra.unique
                            |> List.map (\ss -> symbol :: ss)
                    )

          else
            []
        ]


{-| Create the list of all possible pairs from two other lists.
-}
cross : List a -> List b -> List ( a, b )
cross xs ys =
    xs |> List.concatMap (\x -> ys |> List.map (\y -> ( x, y )))


{-| Transition table
For each combination of state and symbol, there is one entry, namely the list of destination states.
-}
table : NFA state symbol -> List ( state, symbol, List state )
table nfa =
    cross (states nfa) (symbols nfa)
        |> List.map (\( st, sym ) -> ( st, sym, nfa.transitions |> List.filter (\t -> t.from == st && t.symbol == sym) |> List.map .to ))


{-| Wether there is at most one outgoing transition for every combination of state and symbol.
-}
isDeterministic : NFA state symbol -> Bool
isDeterministic nfa =
    nfa |> table |> List.all (\( _, _, l ) -> List.length l <= 1)


{-| Wether there is at least one outgoing transition for every combination of state and symbol.
-}
isComplete : NFA state symbol -> Bool
isComplete nfa =
    nfa |> table |> List.all (\( _, _, l ) -> List.length l >= 1)


{-| Make sure the automaton never gets stuck.
Takes a new state which must be different from existing states.
For each state, adds missing outgoing transitions targeting the new state.
-}
complete : state -> NFA state symbol -> NFA state symbol
complete newState nfa =
    { nfa
        | transitions =
            table nfa
                ++ (nfa |> symbols |> List.map (\s -> ( newState, s, [ newState ] )))
                |> List.concatMap
                    (\( from, symbol, dests ) ->
                        if dests == [] then
                            [ Transition from symbol newState ]

                        else
                            dests |> List.map (\d -> Transition from symbol d)
                    )
    }


eqTable : NFA st sym -> NFA st sym -> List ( ( st, st ), List ( sym, List ( st, st ) ) )
eqTable fsa1 fsa2 =
    let
        syms =
            symbols fsa1 ++ symbols fsa2 |> unique

        aux :
            List ( ( st, st ), List ( sym, List ( st, st ) ) )
            -> List ( st, st )
            -> List ( ( st, st ), List ( sym, List ( st, st ) ) )
        aux acc new =
            case new of
                first :: rest ->
                    if List.Assoc.getFirst first acc |> isJust then
                        aux acc rest

                    else
                        let
                            x : List ( sym, List ( st, st ) )
                            x =
                                syms
                                    |> List.map
                                        (\s ->
                                            let
                                                e1 =
                                                    step s fsa1

                                                e2 =
                                                    step s fsa2
                                            in
                                            ( s, cross e1 e2 )
                                        )

                            acc_ =
                                ( first, x ) :: acc

                            new_ =
                                new ++ (x |> List.concatMap (Tuple.second >> List.filter (flip List.Assoc.getFirst acc >> isJust)))
                        in
                        aux acc_ new_

                _ ->
                    acc
    in
    aux [] [ ( fsa1.start, fsa2.start ) ]


eq : NFA state symbol -> NFA state symbol -> Bool
eq _ _ =
    Debug.todo ""
