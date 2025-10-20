port module Ports exposing (renderDot, selectState, selectSymbol, selectTransition, selectedState, selectedStateStep, selectedSymbol, selectedTransition)

import App exposing (State, Symbol, Transition)
import Automata.NFA as NFA


symbolToString : Symbol -> String
symbolToString symbol =
    symbol |> (Maybe.map String.fromChar >> Maybe.withDefault "ε")


stringToSymbol : String -> Symbol
stringToSymbol str =
    case str |> String.toList |> List.head of
        Just 'ε' ->
            Nothing

        Just s ->
            Just s

        _ ->
            Nothing


transitionToStrings : Transition -> NFA.Transition String String
transitionToStrings { from, to, symbol } =
    { from = from, to = to, symbol = symbol |> Maybe.map String.fromChar |> Maybe.withDefault "ε" }


stringsToTransition : NFA.Transition String String -> Transition
stringsToTransition { from, to, symbol } =
    { from = from, to = to, symbol = stringToSymbol symbol }


stateStepToStrings : ( Maybe Transition, State, Maybe Transition ) -> ( Maybe (NFA.Transition String String), String, Maybe (NFA.Transition String String) )
stateStepToStrings ( from, state, to ) =
    ( from |> Maybe.map transitionToStrings, state, to |> Maybe.map transitionToStrings )


renderDot : ( String, String ) -> Cmd msg
renderDot =
    renderDot_


port renderDot_ : ( String, String ) -> Cmd msg


selectedState : ( String, Maybe State ) -> Cmd msg
selectedState =
    selectedState_


port selectedState_ : ( String, Maybe State ) -> Cmd msg


selectedTransition : ( String, Maybe Transition ) -> Cmd msg
selectedTransition ( graphId, s ) =
    selectedTransition_ ( graphId, s |> Maybe.map transitionToStrings )


port selectedTransition_ : ( String, Maybe (NFA.Transition String String) ) -> Cmd msg


selectedSymbol : ( String, Maybe Symbol ) -> Cmd msg
selectedSymbol ( graphId, s ) =
    selectedSymbol_ ( graphId, s |> Maybe.map symbolToString )


port selectedSymbol_ : ( String, Maybe String ) -> Cmd msg


selectedStateStep : ( String, Maybe ( Maybe Transition, State, Maybe Transition ) ) -> Cmd msg
selectedStateStep ( graphId, s ) =
    selectedStateStep_ ( graphId, s |> Maybe.map stateStepToStrings )


port selectedStateStep_ : ( String, Maybe ( Maybe (NFA.Transition String String), String, Maybe (NFA.Transition String String) ) ) -> Cmd msg


selectState : (String -> Maybe State -> msg) -> Sub msg
selectState msg =
    selectState_ (\( graphId, s ) -> msg graphId s)


port selectState_ : (( String, Maybe State ) -> msg) -> Sub msg


selectTransition : (String -> Maybe Transition -> msg) -> Sub msg
selectTransition msg =
    selectTransition_ (\( graphId, s ) -> msg graphId (s |> Maybe.map stringsToTransition))


port selectTransition_ : (( String, Maybe (NFA.Transition String String) ) -> msg) -> Sub msg


selectSymbol : (String -> Maybe Symbol -> msg) -> Sub msg
selectSymbol msg =
    selectSymbol_ (\( graphId, s ) -> msg graphId (s |> Maybe.map stringToSymbol))


port selectSymbol_ : (( String, Maybe String ) -> msg) -> Sub msg
