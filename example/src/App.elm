module App exposing (..)

import Automata.ENFA as ENFA
import Automata.NFA as NFA
import Dict exposing (Dict)


type alias ENFA =
    ENFA.ENFA String Char


type alias Transition =
    NFA.Transition String (Maybe Char)


type alias State =
    String


type alias Symbol =
    Maybe Char


type alias Step =
    ( Maybe Char, String )


type alias Path =
    List Step


type alias Model =
    { spec : String
    , enfa : Maybe ENFA
    , uis : Dict String Ui
    }


type alias Ui =
    { graphId : String
    , enfa : ENFA
    , playing : Maybe ( Int, Int )
    , input : String
    , generateMaxLength : Int
    , selectedState : Maybe State
    , selectedSymbol : Maybe Symbol
    , selectedTransition : Maybe Transition
    , selectedStateStep : Maybe ( Maybe Transition, State, Maybe Transition )
    }


initUi : Ui
initUi =
    { graphId = ""
    , enfa = { start = "", ends = [], transitions = [] }
    , playing = Nothing
    , input = "000"
    , generateMaxLength = 3
    , selectedState = Nothing
    , selectedSymbol = Nothing
    , selectedTransition = Nothing
    , selectedStateStep = Nothing
    }
