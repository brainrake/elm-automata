module Examples exposing (..)


examples : List ( String, String )
examples =
    [ ( "one"
      , example
      )
    , ( "two"
      , """q0
q0 q2
q0 a q1
q1 b q0
q1 b q2
q2 a q1
    """
      )
    ]


example : String
example =
    """q1
q2 q3
q1 0 q2
q1 0 q1
q1 1 q3
q2 0 q3
q2 0 q2
q2 1 q3
q2 _ q1
"""
