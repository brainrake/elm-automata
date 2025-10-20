module Main exposing (..)

import App exposing (..)
import Automata.DFA as DFA
import Automata.Dot
import Automata.ENFA as ENFA
import Automata.NFA as NFA
import Basics.Extra exposing (flip)
import Browser
import Dict
import Examples
import Html exposing (Html, a, b, button, code, div, h2, h3, h4, h5, input, li, option, p, section, select, small, span, table, td, text, textarea, th, tr, ul)
import Html.Attributes as H exposing (class, classList, cols, href, id, rows, style, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Extra exposing (viewMaybe)
import List.Extra
import Maybe.Extra exposing (isJust)
import Parse
import Ports


type Msg
    = UpdateSpec String
    | UpdateInput String String
    | UpdateGenerateMaxLength String String
    | SelectState String (Maybe State)
    | SelectSymbol String (Maybe Symbol)
    | SelectTransition String (Maybe Transition)
    | SelectStateStep String (Maybe ( Maybe Transition, State, Maybe Transition ))


concatStates : List String -> String
concatStates s =
    if s == [] then
        "∅"

    else
        "{" ++ String.join "," (List.sort s) ++ "}"


init : () -> ( Model, Cmd Msg )
init _ =
    update (UpdateSpec Examples.example)
        { spec = ""
        , enfa = Nothing
        , uis = Dict.empty
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateUi graphId f =
            { model | uis = model.uis |> Dict.update graphId (Maybe.map f) }
    in
    case msg of
        UpdateSpec spec ->
            case Parse.parse spec of
                Just enfa ->
                    let
                        uis =
                            [ ( "main", identity )
                            , ( "toNFA", ENFA.toNFA >> ENFA.fromNFA )
                            , ( "completeNFA", ENFA.toNFA >> NFA.complete "zzz" >> ENFA.fromNFA )
                            , ( "toDFA", ENFA.toNFA >> DFA.fromNFA concatStates >> ENFA.fromNFA )
                            , ( "completeDFA", ENFA.toNFA >> DFA.fromNFA concatStates >> NFA.complete "zzz" >> ENFA.fromNFA )
                            ]
                                |> Dict.fromList
                                |> Dict.map (\k f -> { initUi | graphId = k, enfa = f enfa })
                    in
                    ( { model | spec = spec, enfa = Just enfa, uis = uis }
                    , Cmd.batch (uis |> Dict.map (\k v -> Ports.renderDot ( k, efsaToDot v.enfa )) |> Dict.values)
                    )

                Nothing ->
                    ( { model | spec = spec }, Cmd.none )

        UpdateInput graphId input ->
            ( updateUi graphId (\ui -> { ui | input = input }), Cmd.none )

        UpdateGenerateMaxLength graphId s ->
            ( updateUi graphId (\ui -> { ui | generateMaxLength = s |> String.toInt |> Maybe.withDefault ui.generateMaxLength }), Cmd.none )

        SelectState graphId node ->
            ( updateUi graphId (\ui -> { ui | selectedState = node }), Ports.selectedState ( graphId, node ) )

        SelectSymbol graphId s ->
            ( updateUi graphId (\ui -> { ui | selectedSymbol = s }), Ports.selectedSymbol ( graphId, s ) )

        SelectTransition graphId s ->
            ( updateUi graphId (\ui -> { ui | selectedTransition = s }), Ports.selectedTransition ( graphId, s ) )

        SelectStateStep graphId s ->
            ( updateUi graphId (\ui -> { ui | selectedNodeStep = s }), Ports.selectedStateStep ( graphId, s ) )


view : Model -> Html Msg
view model =
    div []
        [ section []
            [ h2 [] [ text "Finite State Automata ", small [] [ text "An Explorable Explanation" ] ]
            ]
        , section []
            [ h3 []
                [ div [ class "columns" ]
                    [ span [] [ text "Specification " ]

                    -- , select [] [ option [] [ text "Load Example" ] ]
                    ]
                ]
            , div [ class "columns" ]
                [ div []
                    [ textarea [ value model.spec, onInput UpdateSpec, rows 10, cols 14 ] []
                    , div [] [ text "Parsed: ", viewBool (isJust (Parse.parse model.spec)) ]
                    ]
                , div [ class "help" ]
                    [ p []
                        [ text "Enter your "
                        , a [ href "https://en.wikipedia.org/wiki/Finite-state_machine" ]
                            [ text "Finite State Machine" ]
                        , text " specification here."
                        ]
                    , p []
                        [ text "The first line is the start state. "
                        , text "The second line are the end states separated by spaces. "
                        , text "The remaining lines are transitions: each one is a from-state, a single character symbol, and to-state, separated by spaces."
                        ]
                    , p []
                        [ text "The symbol for empty input "
                        , span [ class "symbolLabel " ] [ text "ε" ]
                        , text " (epsilon) is written as an "
                        , span [ class "symbolLabel " ] [ text "_" ]
                        , text "(underscore). "
                        , text "Lines starting with "
                        , span [ class "symbolLabel " ] [ text "#" ]
                        , text " are ignored."
                        ]
                    ]
                ]
            ]
        , div []
            [ model.enfa
                |> Maybe.map (\a -> viewAutomatonDetails model a)
                |> Maybe.withDefault (text "Parse Error.")
            ]
        ]


viewInput : Html Msg
viewInput =
    div [] []


viewStateEl : List (Html.Attribute Msg) -> Ui -> State -> Html Msg
viewStateEl attrs ui state =
    span
        ([ class "stateLabel"
         , classList
            [ ( "start", state == ui.enfa.start )
            , ( "end", List.member state ui.enfa.ends )
            ]
         ]
            ++ attrs
        )
        [ text state ]


viewState : Ui -> State -> Html Msg
viewState ui state =
    viewStateEl
        [ onClick (SelectState ui.graphId (Just state))
        , onMouseEnter (SelectState ui.graphId (Just state))
        , onMouseLeave (SelectState ui.graphId Nothing)
        , classList
            [ ( "selected", ui.selectedState == Just state || (ui.selectedNodeStep |> Maybe.map (\( _, s, _ ) -> s == state) |> Maybe.withDefault False) )
            , ( "from", Maybe.map .from ui.selectedTransition == Just state )
            , ( "to", Maybe.map .to ui.selectedTransition == Just state )
            ]
        ]
        ui
        state


viewStates : Ui -> List State -> Html Msg
viewStates ui states =
    Html.span [ class "inline-flex" ] (states |> List.sortBy (String.replace "," "}") |> List.map (viewState ui))


viewSymbol : Ui -> Maybe Char -> Html Msg
viewSymbol { graphId, selectedSymbol } sym =
    span
        [ class "symbolLabel"
        , onMouseEnter (SelectSymbol graphId (Just sym))
        , onMouseLeave (SelectSymbol graphId Nothing)
        , classList [ ( "selected", selectedSymbol == Just sym ), ( "epsilon", Maybe.map String.fromChar sym == Nothing ) ]
        ]
        [ text (sym |> Maybe.map String.fromChar |> Maybe.withDefault "ε") ]


viewSymbols : Ui -> List (Maybe Char) -> Html Msg
viewSymbols ui syms =
    Html.span [] (syms |> List.sortBy (Maybe.withDefault ' ') |> List.map (viewSymbol ui))


viewAutomaton : Ui -> Html Msg
viewAutomaton ui =
    let
        paths =
            ui.enfa |> ENFA.allPaths (ui.input |> String.toList)

        endPaths =
            paths |> List.filter (List.Extra.last >> Maybe.map (Tuple.second >> flip List.member ui.enfa.ends) >> Maybe.withDefault False)

        accepted =
            List.length endPaths > 0

        thePaths =
            if accepted then
                endPaths

            else
                paths

        generated =
            ui.enfa |> ENFA.generate ui.generateMaxLength |> List.Extra.unique |> List.sort |> List.reverse |> List.sortBy List.length |> List.reverse

        nfa =
            ui.enfa |> ENFA.toNFA
    in
    div [ class "columns wrap" ]
        [ div []
            [ h5 [] [ text "States and Symbols" ]
            , table []
                [ tr [] [ td [] [ text "States: " ], td [] [ NFA.states ui.enfa |> viewStates ui ] ]
                , tr [] [ td [] [ text "Symbols: " ], td [] [ ui.enfa |> NFA.symbols |> viewSymbols ui ] ]
                , tr [] [ td [] [ text "Start: " ], td [] [ ui.enfa.start |> viewState ui ] ]
                , tr [] [ td [] [ text "Ends: " ], td [] [ ui.enfa.ends |> viewStates ui ] ]
                ]
            , p [] []
            , h5 [] [ text "Properties" ]
            , table []
                [ tr [] [ td [] [ text "ε-free" ], td [] [ viewBool (ENFA.isEpsilonFree ui.enfa) ] ]
                , tr [] [ td [] [ text "Deterministic" ], td [] [ viewBool (nfa |> NFA.isDeterministic) ] ]
                , tr [] [ td [] [ text "Complete" ], td [] [ viewBool (nfa |> NFA.isComplete) ] ]
                ]
            ]
        , div []
            [ h5 [] [ text "Transitions " ]
            , viewTransitions ui
            ]
        , div []
            [ h5 [] [ text "Graph" ]
            , div [ id ui.graphId ] []
            ]
        , div []
            [ h5 [] [ text "Run" ]
            , div [] [ text "Input:" ]
            , div [] [ input [ value ui.input, onInput (UpdateInput ui.graphId) ] [] ]
            , p [] [ text "Accepted: ", viewBool accepted ]
            , p []
                [ b [] [ text (String.fromInt (List.length thePaths)) ]
                , text " possible "
                , text <|
                    if accepted then
                        "accepted "

                    else
                        ""
                , text "paths:"
                ]
            , div [ class "paths border" ]
                (thePaths |> List.map (viewPath ui))
            , h5 [] [ text "Generate" ]
            , p []
                [ text "Max length: "
                , input [ type_ "number", H.max "10", H.min "1", value (ui.generateMaxLength |> String.fromInt), onInput (UpdateGenerateMaxLength ui.graphId) ] []
                ]
            , p [] [b [] [text <| String.fromInt <| List.length generated], text " examples:"  ]
            , div [ class "columns" ]
                [ div [ class "generate border" ]
                    (generated |> List.map (\s -> div [] [ code [ onClick (UpdateInput ui.graphId (String.fromList s)) ] [ text (String.fromList s) ] ]))
                ]
            ]
        ]


viewStepState : Ui -> Maybe Transition -> State -> Maybe Transition -> Html Msg
viewStepState ui prevE state nextE =
    viewStateEl
        [ onClick (SelectStateStep ui.graphId (Just ( prevE, state, nextE )))
        , onMouseEnter (SelectStateStep ui.graphId (Just ( prevE, state, nextE )))
        , onMouseLeave (SelectStateStep ui.graphId Nothing)
        , classList
            [ ( "start", state == ui.enfa.start )
            , ( "end", List.member state ui.enfa.ends )
            ]
        ]
        ui
        state


viewTransitionState : Ui -> Maybe Transition -> State -> Maybe Transition -> Html Msg
viewTransitionState ui prevE state nextE =
    let
        isSelectedNodeStep =
            ui.selectedNodeStep |> Maybe.map (\( p, _, n ) -> p == prevE || n == nextE) |> Maybe.withDefault False

        isSelectedNode =
            ui.selectedState == Just state
    in
    viewStateEl
        [ onClick (SelectState ui.graphId (Just state))
        , onMouseEnter (SelectState ui.graphId (Just state))
        , onMouseLeave (SelectState ui.graphId Nothing)
        , classList
            [ ( "selected", isSelectedNode || isSelectedNodeStep )
            , ( "from", Maybe.map .from ui.selectedTransition == Just state )
            , ( "to", Maybe.map .to ui.selectedTransition == Just state )
            , ( "start", state == ui.enfa.start )
            , ( "end", List.member state ui.enfa.ends )
            ]
        ]
        ui
        state


viewPath : Ui -> Path -> Html Msg
viewPath ui path =
    div []
        --button [] [ text "▶" ]
        --::
        (span [] [ text "▶ " ]
            :: (viewStepState
                    { ui | selectedTransition = Nothing, selectedState = Nothing }
                    Nothing
                    ui.enfa.start
                    (path |> List.head |> Maybe.map (\( sym, st ) -> { from = ui.enfa.start, symbol = sym, to = st }))
                    :: (path
                            |> List.foldl (\( s, to ) acc -> { from = acc |> List.head |> Maybe.map .to |> Maybe.withDefault ui.enfa.start, symbol = s, to = to } :: acc) []
                            |> List.foldl (\e acc -> ( e, acc |> List.head |> Maybe.map Tuple.first ) :: acc) []
                            |> List.concatMap
                                (\( e, nextE ) ->
                                    [ viewTransitionSymbolEl
                                        [ onClick (SelectTransition ui.graphId (Just e))
                                        , onMouseEnter (SelectTransition ui.graphId (Just e))
                                        , onMouseLeave (SelectTransition ui.graphId Nothing)
                                        ]
                                        { ui | selectedTransition = Nothing, selectedNodeStep = Nothing }
                                        e
                                    , viewStepState ui (Just e) e.to nextE
                                    ]
                                )
                       )
               )
        )


viewEpsilonTable : Ui -> Html Msg
viewEpsilonTable ({ enfa } as ui) =
    table [ class "inner-border" ]
        (tr []
            [ th [] []
            , th [] [ code [] [ text "ε" ] ]
            , th [ class "columns" ] [ div [] [ text "Δ(", code [] [ text "ε" ], text ")" ] ]
            ]
            :: List.map
                (\st ->
                    tr []
                        [ td [] [ viewStateEl [] ui st ]
                        , td [] [ viewStates ui (NFA.step Nothing { enfa | start = st }) ]
                        , td [] [ viewStates ui (ENFA.emptyStep { enfa | start = st }) ]
                        ]
                )
                (NFA.states enfa)
        )


viewAutomatonDetails : Model -> ENFA -> Html Msg
viewAutomatonDetails model enfa =
    let
        ui =
            model.uis |> Dict.get "main" |> Maybe.withDefault { initUi | graphId = "main" }
    in
    div (model.spec |> Parse.parse |> Maybe.map (always []) |> Maybe.withDefault [ style "opacity" "30%" ])
        [ section []
            [ h3 [] [ text "Definition" ]
            , p [] [ text "The specification results in the following Automaton. The tables and graphs are interactive, hover or tap elements to highlight them." ]
            , model.uis |> Dict.get "main" |> viewMaybe viewAutomaton
            ]
        , section [] [ h3 [] [ text "Transforms" ] ]
        , section []
            [ h4 [] [ text "ε-NFA to NFA" ]
            , p []
                [ text "Convert to "
                , a [ href "https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton" ]
                    [ text "Nondeterministic Finite Automaton" ]
                , text " without empty symbol "
                , code [] [ text "ε" ]
                , text "."
                ]
            , div [ class "columns" ]
                [ div []
                    [ viewEpsilonTable ui ]
                , div [ class "help" ]
                    [ p []
                        [ text "Fill out the table to list states reachable via "
                        , text "Δ("
                        , code [] [ text "ε" ]
                        , text "), meaning"
                        , text " 0 or more "
                        , code [] [ text "ε" ]
                        , text "s."
                        ]
                    , p []
                        [ text "For each state, take the non-"
                        , code [] [ text "ε" ]
                        , text " outgoing transitions of all states reachable via "
                        , text "Δ("
                        , code [] [ text "ε" ]
                        , text ")."
                        ]
                    ]
                ]
            , div [] [ model.uis |> Dict.get "toNFA" |> viewMaybe viewAutomaton ]
            ]
        , model.uis
            |> Dict.get "completeNFA"
            |> viewMaybe
                (\comleteUi ->
                    section []
                        [ h4 [] [ text "Complete NFA" ]
                        , p []
                            [ text "Ensure all states have outgoing transitions for all symbols. Add a new state "
                            , viewState comleteUi "zzz"
                            , text " as the target for new transitions."
                            ]
                        , div [] [ viewAutomaton comleteUi ]
                        ]
                )
        , section []
            [ h4 [] [ text "NFA to DFA" ]
            , p [] [ text "Make the automaton deterministic." ]
            , p []
                [ text "TODO" ]
            , div [] [ model.uis |> Dict.get "toDFA" |> viewMaybe viewAutomaton ]
            ]
        , model.uis
            |> Dict.get "completeDFA"
            |> viewMaybe
                (\comleteUi ->
                    section []
                        [ h4 [] [ text "Complete DFA" ]
                        , p []
                            [ text "Ensure all states have outgoing transitions for all symbols. Add a new state "
                            , viewState comleteUi "zzz"
                            , text " as the target for new transitions."
                            ]
                        , div [] [ viewAutomaton comleteUi ]
                        ]
                )
        , section []
            [ h4 [] [ text "Minify DFA" ]
            , p [] [ text "" ]
            , div [] [ text "TODO" ]
            ]
        , section []
            [ h4 [] [ text "Equivalence" ]
            ]
        ]


viewTransitions : Ui -> Html Msg
viewTransitions ui =
    table [ class "transitions" ]
        (ui.enfa.transitions
            |> List.sortBy (\t -> ( t.from |> String.replace "," "}", t.symbol |> Maybe.withDefault ' ', t.to |> String.replace "," "}" ))
            |> List.map
                (\e ->
                    tr
                        [ class "transition"
                        , classList
                            [ ( "selected", ui.selectedTransition == Just e )
                            , ( "from", (ui.selectedNodeStep |> Maybe.andThen (\( x, _, _ ) -> x)) == Just e )
                            , ( "to", (ui.selectedNodeStep |> Maybe.andThen (\( _, _, x ) -> x)) == Just e )
                            ]
                        , onClick (SelectTransition ui.graphId (Just e))
                        , onMouseEnter (SelectTransition ui.graphId (Just e))
                        , onMouseLeave (SelectTransition ui.graphId Nothing)
                        ]
                        [ td [] [ viewTransitionState ui Nothing e.from (Just e) ]
                        , td [] [ viewTransitionSymbolEl [] ui e ]
                        , td [] [ viewTransitionState ui (Just e) e.to Nothing ]
                        ]
                )
        )


viewTransitionSymbolEl : List (Html.Attribute Msg) -> Ui -> Transition -> Html Msg
viewTransitionSymbolEl attrs ui t =
    span
        (attrs
            ++ [ class "symbolLabel"
               , classList
                    [ ( "selected", ui.selectedSymbol == Just t.symbol || (ui.selectedTransition == Just t) )
                    , ( "epsilon", t.symbol == Nothing )

                    -- , ( "from", Just t == (ui.selectedNodeStep |> Maybe.andThen (\( x, _, _ ) -> x)) )
                    -- , ( "to", Just t == (ui.selectedNodeStep |> Maybe.andThen (\( _, _, x ) -> x)) )
                    ]
               ]
        )
        [ text (t.symbol |> Maybe.map String.fromChar |> Maybe.withDefault "ε") ]


viewBool : Bool -> Html msg
viewBool b =
    let
        ( cls, label ) =
            if b then
                ( "true", "yes" )

            else
                ( "false", "no" )
    in
    span [ class "bool", class cls ] [ text label ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \m -> { title = "", body = [ view m ] }
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Ports.selectState SelectState
                    , Ports.selectTransition SelectTransition
                    , Ports.selectSymbol SelectSymbol
                    ]
        }


efsaToDot : ENFA -> String
efsaToDot efa =
    let
        nodeClass n =
            String.join " "
                [ if efa.start == n then
                    "start"

                  else
                    ""
                , if List.member n efa.ends then
                    "end"

                  else
                    ""
                ]
    in
    Automata.Dot.outputWithStylesAndAttributes
        Automata.Dot.defaultStyles
        (\n -> Dict.fromList [ ( "label", n ), ( "class", nodeClass n ) ])
        (\e -> Dict.fromList [ ( "label", e |> Maybe.map String.fromChar |> Maybe.withDefault "ε" ) ])
        efa
