module Experimenal.RippleCarryAdder exposing (..)

import Bitwise


andGate : Int -> Int -> Int
andGate a b =
    Bitwise.and a b


orGate : Int -> Int -> Int
orGate a b =
    Bitwise.or a b


bitInvertor : Int -> Int
bitInvertor a =
    case a of
        1 ->
            0

        0 ->
            1

        _ ->
            -1


halfAdder : Int -> Int -> { sum : Int, carry : Int }
halfAdder a b =
    { sum = orGate a b |> andGate (andGate a b |> bitInvertor)
    , carry = andGate a b
    }


fullAdder : Int -> Int -> Int -> { sum : Int, carry : Int }
fullAdder a b cin =
    let
        firstResult =
            cin |> halfAdder b

        secondResult =
            firstResult.sum |> halfAdder a

        finalCarry =
            secondResult.carry |> orGate firstResult.carry
    in
    { sum = secondResult |> .sum
    , carry = finalCarry
    }


type alias Binary =
    { d0 : Int
    , d1 : Int
    , d2 : Int
    , d3 : Int
    }


rippleCarryAdder : Binary -> Binary -> Int -> { carry : Int, sum0 : Int, sum1 : Int, sum2 : Int, sum3 : Int }
rippleCarryAdder a b carryIn =
    let
        firstResult =
            fullAdder a.d3 b.d3 carryIn

        secondResult =
            fullAdder a.d2 b.d2 firstResult.carry

        thirdResult =
            fullAdder a.d1 b.d1 secondResult.carry

        finalResult =
            fullAdder a.d0 b.d0 thirdResult.carry
    in
    { carry = finalResult.carry
    , sum0 = finalResult.sum
    , sum1 = thirdResult.sum
    , sum2 = secondResult.sum
    , sum3 = firstResult.sum
    }
