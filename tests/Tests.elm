module Tests exposing (..)

import Automata.ENFA as ENFA exposing (ENFA)
import Automata.NFA as FSA exposing (NFA, Transition)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


example : ENFA String Char
example =
    { start = "q1"
    , ends = [ "q3" ]
    , transitions =
        [ Transition "q1" Nothing "q2"
        , Transition "q2" (Just '0') "q3"
        ]
    }


suite : Test
suite =
    describe "EFSA" <|
        [ describe "emptySteps"
            [ test "example" <|
                \_ ->
                    ENFA.emptyStep example
                        |> Expect.equalLists [ "q1", "q2" ]
            ]
        , describe "toFSA"
            [ test "example" <|
                \_ ->
                    (ENFA.toNFA example).transitions
                        |> Expect.equal
                            [ Transition "q1" '0' "q3"
                            , Transition "q2" '0' "q3"
                            ]
            ]
        ]
