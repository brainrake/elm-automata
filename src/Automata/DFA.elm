module Automata.DFA exposing (..)

import Automata.NFA as NFA exposing (..)
import Basics.Extra exposing (flip)
import Html exposing (a)
import List.Extra


{-| Deterministic Finite-state Automaton
-}
type alias DFA state symbol =
    NFA state symbol


{-| Transform NFA to DFA using the powerset construction.
-}
fromNFA : (List state -> newState) -> NFA state symbol -> DFA newState symbol
fromNFA combineStates nfa =
    let
        ends =
            List.Extra.subsequences (states nfa) |> List.filter (List.any (flip List.member nfa.ends)) |> List.map combineStates

        transitionsFrom : List state -> List (Transition (List state) symbol)
        transitionsFrom states =
            symbols nfa
                |> List.map
                    (\sym ->
                        states
                            |> List.concatMap (\st -> NFA.step sym { nfa | start = st })
                            |> List.Extra.unique
                            |> (\s -> { from = states, symbol = sym, to = s })
                    )
                |> List.filter (.to >> (/=) [])

        combineTransitionStates : Transition (List state) symbol -> Transition newState symbol
        combineTransitionStates t =
            { from = combineStates t.from, symbol = t.symbol, to = combineStates t.to }

        aux : List (List state) -> List (Transition newState symbol) -> List (Transition newState symbol)
        aux stateSets acc =
            case stateSets of
                first :: rest ->
                    let
                        ts =
                            transitionsFrom first

                        newStateSets =
                            ts |> List.map .to |> List.filter (\s -> not (List.member (combineStates s) (acc |> List.map .from)))
                    in
                    aux
                        (rest ++ newStateSets)
                        (acc ++ (ts |> List.map combineTransitionStates))

                [] ->
                    acc

        transitions : List (Transition newState symbol)
        transitions =
            aux [ [ nfa.start ] ] [] |> List.Extra.unique
    in
    { start = combineStates [ nfa.start ]
    , ends = ends |> List.filter (\end -> List.member end (transitions |> List.concatMap (\t -> [ t.from, t.to ])))
    , transitions = transitions
    }


{-| States that are reachable from the start state.
-}
reachable : NFA state symbol -> List state
reachable nfa =
    let
        aux : List state -> List state -> List state
        aux acc new =
            case new of
                first :: rest ->
                    if List.member first acc then
                        aux acc rest

                    else
                        let
                            newStates =
                                nfa.transitions |> List.filter (\t -> t.from == first) |> List.map .to
                        in
                        aux (first :: acc) (rest ++ newStates)

                [] ->
                    acc
    in
    aux [] [ nfa.start ]


{-| States that are not reachable from the start state.
-}
unreachable : NFA state symbol -> List state
unreachable nfa =
    states nfa |> List.filter (not << flip List.member (reachable nfa))


{-| States from which an end state is reachable.
-}
productive : NFA state symbol -> List state
productive nfa =
    let
        aux : List state -> List state -> List state
        aux acc new =
            case new of
                first :: rest ->
                    if List.member first acc then
                        aux acc rest

                    else
                        let
                            newStates =
                                nfa.transitions |> List.filter (\t -> t.to == first) |> List.map .from
                        in
                        aux (first :: acc) (rest ++ newStates)

                [] ->
                    acc
    in
    aux [] nfa.ends


{-| States from which an end state is not reachable.
-}
unproductive : NFA state symbol -> List state
unproductive nfa =
    states nfa |> List.filter (not << flip List.member (productive nfa))


prune : DFA state symbol -> DFA state symbol
prune dfa =
    let
        useful =
            reachable dfa |> List.filter (flip List.member (productive dfa))

        ends =
            dfa.ends |> List.filter (flip List.member useful)

        transitions =
            dfa.transitions |> List.filter (\t -> List.member t.from useful && List.member t.to useful)
    in
    { start = dfa.start
    , ends = ends
    , transitions = transitions
    }


minimize : DFA state symbol -> DFA state symbol
minimize dfa_ =
    let
        dfa =
            prune dfa_

        allSyms =
            symbols dfa

        matrix : List (List ( state, state ))
        matrix =
            states dfa |> List.map (\s1 -> states dfa |> List.map (Tuple.pair s1))

        different : ( state, state ) -> Bool
        different ( s1, s2 ) =
            ended { dfa | start = s1 } /= ended { dfa | start = s2 }

        nextDifferent : List (List ( ( state, state ), Bool )) -> ( state, state ) -> Bool
        nextDifferent d ( s1, s2 ) =
            allSyms
                |> List.any
                    (\sym ->
                        cross
                            (NFA.step sym { dfa | start = s1 })
                            (NFA.step sym { dfa | start = s1 })
                            |> List.any different
                    )

        d0 : List (List ( ( state, state ), Bool ))
        d0 =
            matrix |> List.map (List.map (\p -> ( p, different p )))

        dS : List (List ( ( state, state ), Bool )) -> List (List ( ( state, state ), Bool ))
        dS d =
            d |> List.map (List.map (\( p, dif ) -> ( p, dif || nextDifferent d p )))

        fix : (a -> a) -> a -> a
        fix f x =
            let
                next =
                    f x
            in
            if next == x then
                x

            else
                fix f next

        dN : List (List ( ( state, state ), Bool ))
        dN =
            fix dS d0

        indistinguishable : List ( state, state )
        indistinguishable =
            dN |> List.concatMap (List.filter Tuple.second >> List.map Tuple.first) |> (\i -> i |> List.filter (\( s1, s2 ) -> not (List.member ( s2, s1 ) i)))

        toRemove : List state
        toRemove =
            indistinguishable |> List.map Tuple.second
    in
    { start = dfa.start
    , ends = dfa.ends |> List.filter (\e -> not (List.member e toRemove))
    , transitions = dfa.transitions |> List.filter (\t -> not (List.member t.from toRemove) && not (List.member t.to toRemove))
    }
        |> prune
