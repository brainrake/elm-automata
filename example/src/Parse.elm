module Parse exposing (..)

import Automata.ENFA exposing (ENFA)
import Automata.NFA as NFA
import Maybe.Extra


parse : String -> Maybe (ENFA String Char)
parse str =
    let
        lines =
            str |> String.trim |> String.split "\n" |> List.filter (not << String.startsWith "#") |> List.map String.trim |> List.filter ((/=) "")

        startLabel =
            lines |> List.head |> Maybe.map String.trim

        endLabels =
            lines |> List.drop 1 |> List.head |> Maybe.map String.words

        transition s =
            case String.words s of
                [ from, symbol, to ] ->
                    if String.length symbol == 1 then
                        symbol
                            |> String.toList
                            |> List.head
                            |> Maybe.map
                                (\sym ->
                                    { from = from
                                    , symbol =
                                        if sym == '_' then
                                            Nothing

                                        else
                                            Just sym
                                    , to = to
                                    }
                                )

                    else
                        Nothing

                _ ->
                    Nothing

        transitions =
            lines |> List.drop 2 |> List.map transition |> Maybe.Extra.combine
    in
    Maybe.map3 NFA.NFA startLabel endLabels transitions


parseLabel : String -> Maybe (Maybe Char)
parseLabel label =
    label
        |> String.toList
        |> List.head
        |> Maybe.map
            (\c ->
                if c == 'ε' then
                    Nothing

                else
                    Just c
            )
