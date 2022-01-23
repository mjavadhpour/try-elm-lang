module Experimental.RippleCarryAdderTests exposing (..)

import Expect exposing (Expectation)
import Experimental.RippleCarryAdder exposing (..)
import Fuzz exposing (..)
import Test exposing (..)


inverterTests : Test
inverterTests =
    describe "Invertor"
        [ test "output is 0 when the input is 1" <|
            \() ->
                bitInvertor 0
                    |> Expect.equal 1
        , test "output is 1 when the input is 0" <|
            \() ->
                bitInvertor 1
                    |> Expect.equal 0
        ]


andGateTests : Test
andGateTests =
    describe "AND gate"
        [ test "output is 0 when both inputs are 0" <|
            \() ->
                andGate 0 0
                    |> Expect.equal 0
        , test "output is 0 when the first input is 0" <|
            \() ->
                andGate 0 1
                    |> Expect.equal 0
        , test "output is 0 when the second input is 0" <|
            \() ->
                andGate 1 0
                    |> Expect.equal 0
        , test "output is 1 when both inputs are 1" <|
            \() ->
                andGate 1 1
                    |> Expect.equal 1
        ]


orGateTests : Test
orGateTests =
    describe "OR gate"
        [ test "output is 0 when both inputs are 0" <|
            \() ->
                orGate 0 0
                    |> Expect.equal 0
        , test "output is 0 when the first input is 0" <|
            \() ->
                orGate 0 1
                    |> Expect.equal 1
        , test "output is 0 when the second input is 0" <|
            \() ->
                orGate 1 0
                    |> Expect.equal 1
        , test "output is 1 when both inputs are 1" <|
            \() ->
                orGate 1 1
                    |> Expect.equal 1
        ]


halfAdderTests : Test
halfAdderTests =
    describe "Half adder"
        [ describe "when both inputs are 0"
            [ test "sum and carry-out are 0" <|
                \() ->
                    halfAdder 0 0
                        |> Expect.equal { carry = 0, sum = 0 }
            ]
        , describe "when both inputs are 1"
            [ test "sum is 0 carry-out is 1" <|
                \() ->
                    halfAdder 1 1
                        |> Expect.equal { carry = 1, sum = 0 }
            ]
        , describe "when inputs is not same"
            [ describe "and the 1st input is 0 and the 2nd input is 1"
                [ test "sum is 1 and carry-out is 0" <|
                    \() ->
                        halfAdder 0 1
                            |> Expect.equal { carry = 0, sum = 1 }
                ]
            , describe "and the 1st input is 1 and the 2nd input is 0"
                [ test "sum is 1 and carry-out is 0" <|
                    \() ->
                        halfAdder 1 0
                            |> Expect.equal { carry = 0, sum = 1 }
                ]
            ]
        ]


fullAdderTests : Test
fullAdderTests =
    describe "Full adder"
        [ describe "when both inputs are 0"
            [ describe "and carry-in is 0"
                [ test "both sum and carry-out are 0" <|
                    \() ->
                        fullAdder 0 0 0
                            |> Expect.equal { carry = 0, sum = 0 }
                ]
            , describe "but carry-out is 1"
                [ test "sum is 1 and carry-out is 0" <|
                    \() ->
                        fullAdder 0 0 1
                            |> Expect.equal { carry = 0, sum = 1 }
                ]
            ]
        , describe "when the 1st input is 0"
            [ describe "and the 2nd input is 1"
                [ describe "and carry-in is 0"
                    [ test "sum is 1 and carry-out is 0" <|
                        \() ->
                            fullAdder 0 1 0
                                |> Expect.equal { carry = 0, sum = 1 }
                    ]
                , describe "and carry-in is 1"
                    [ test "sum is 0 and carry-out is 1" <|
                        \() ->
                            fullAdder 0 1 1
                                |> Expect.equal { carry = 1, sum = 0 }
                    ]
                ]
            ]
        , describe "when the 1st input is 1"
            [ describe "and the 2nd input is 0"
                [ describe "and carry-in is 0"
                    [ test "sum is 1 and carry-out is 0" <|
                        \() ->
                            fullAdder 1 0 0
                                |> Expect.equal { carry = 0, sum = 1 }
                    ]
                , describe "and carry-in is 1"
                    [ test "sum is 0 and carry-out is 1" <|
                        \() ->
                            fullAdder 1 0 1
                                |> Expect.equal { carry = 1, sum = 0 }
                    ]
                ]
            , describe "and the 2nd input is 1"
                [ describe "and carry-in is 0"
                    [ test "sum is 0 and carry-out is 1" <|
                        \() ->
                            fullAdder 1 1 0
                                |> Expect.equal { carry = 1, sum = 0 }
                    ]
                , describe "and carry-in is 1"
                    [ test "sum is 1 and carry-out is 1" <|
                        \() ->
                            fullAdder 1 1 1
                                |> Expect.equal { carry = 1, sum = 1 }
                    ]
                ]
            ]
        ]


rippleCarryAdderTests : Test
rippleCarryAdderTests =
    describe "4-bit ripple carry adder"
        [ describe "given two binary numbers and a carry-in digit"
            [ test "returns the sum of those numbers and a carry-out digit" <|
                \_ ->
                    rippleCarryAdder (Binary 1 0 0 1) (Binary 1 1 0 1) 1
                        |> Expect.equal (BinarySum 1 0 1 1 1)
            ]
        , describe "when the 1st input is 1111, and the 2nd input is 1111"
            [ test "and carry-in is 0, the output is 11110" <|
                \_ ->
                    rippleCarryAdder (Binary 1 1 1 1) (Binary 1 1 1 1) 0
                        |> Expect.equal (BinarySum 1 1 1 1 0)
            , test "and carry-in is 1, the output is 11111" <|
                \_ ->
                    rippleCarryAdder (Binary 1 1 1 1) (Binary 1 1 1 1) 1
                        |> Expect.equal (BinarySum 1 1 1 1 1)
            ]
        , describe "when the 1st input is 0000, and the 2nd input is 0000"
            [ test "and carry-in is 0, the output is 0000" <|
                \_ ->
                    rippleCarryAdder (Binary 0 0 0 0) (Binary 0 0 0 0) 0
                        |> Expect.equal (BinarySum 0 0 0 0 0)
            , test "and carry-in is 1, the output is 0001" <|
                \_ ->
                    rippleCarryAdder (Binary 0 0 0 0) (Binary 0 0 0 0) 1
                        |> Expect.equal (BinarySum 0 0 0 0 1)
            ]
        ]
