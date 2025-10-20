module Automata.DFA exposing (..)

import Automata.NFA as NFA exposing (..)
import Basics.Extra exposing (flip)
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
                        aux acc (rest ++ newStates)

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
                        aux acc (rest ++ newStates)

                [] ->
                    acc
    in
    aux [] nfa.ends


{-| States from which an end state is not reachable.
-}
unproductive : NFA state symbol -> List state
unproductive nfa =
    states nfa |> List.filter (not << flip List.member (productive nfa))


minify : DFA state symbol -> DFA state symbol
minify dfa =
    -- Construct an nxn matrix D(0) such that the entry D(0)i,j is TRUE if one of the states i, j is final while the other is not. The entry is FALSE otherwise (both i, j are final or both non-final states);
    -- Given matrix D(k) we can construct matrix D(k+1) as follows: D(k+1)i,j is TRUE if D(k)i,j was TRUE or there is an input a such that starting from state i with input a takes us to a state i' and stating from state j with input a takes us to a state j' such that D(k)i',j' is TRUE. Otherwise, the entry D(k+1)i,j remains marked FALSE.
    -- Repeat the previous step until D(k+1)i,j is the same as D(k)i,j. Call this final matrix D.
    -- We now know that state i is indistinguishable from state j if and only if Di,j is FALSE - join together the indistinguishable states
    let
        matrix : List (List ( state, state ))
        matrix =
            states dfa |> List.map (\s1 -> states dfa |> List.map (Tuple.pair s1))

        different : ( state, state ) -> Bool
        different ( s1, s2 ) =
            ended { dfa | start = s1 } /= ended { dfa | start = s2 }

        d0 : List (List Bool)
        d0 =
            matrix |> List.map (List.map different)

        incoming : state -> List (Transition state symbol)
        incoming state =
            []

        dS d =
            0
    in
    dfa
