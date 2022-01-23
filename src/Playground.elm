module Playground exposing (..)

import Html


escapeEarth : Float -> Float -> String -> String
escapeEarth myVelocity mySpeed fuelStatus =
    let
        escapeVelocityInKmPerSec =
            11.186

        orbitalSpeedInKmPerSec =
            7.67

        whereToLand =
            if fuelStatus == "low" then
                "Land on droneship"

            else
                "Land on launchpad"
    in
    if myVelocity > escapeVelocityInKmPerSec then
        "Godspeed"

    else if mySpeed == orbitalSpeedInKmPerSec then
        "Stay in orbit"

    else
        whereToLand


computeSpeed : Float -> Float -> Float
computeSpeed distance time =
    distance / time


computeTime : number -> number -> number
computeTime startTime endTime =
    endTime - startTime


weekday : Int -> String
weekday dayInNumber =
    case dayInNumber of
        0 ->
            "Sunday"

        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "wednesday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        6 ->
            "Saturday"

        _ ->
            "Unknown Day"


hashtag : Int -> String
hashtag dayInNumber =
    case weekday dayInNumber of
        "Sunday" ->
            "#SinDay"

        "Monday" ->
            "#MondayBlues"

        "Tuesday" ->
            "#TakeMeBackTuesday"

        "Wednesday" ->
            "#HumpDay"

        "Thursday" ->
            "#ThrowbackThursday"

        "Friday" ->
            "#FlashbackFriday"

        "Saturday" ->
            "#Caturday"

        _ ->
            "#Whatever"


type Greeting a
    = Howdy
    | Hola
    | Namaste a
    | NumericalHi Int Int


sayHello : Greeting a -> String
sayHello message =
    case message of
        Howdy ->
            ""

        Hola ->
            ""

        Namaste _ ->
            ""

        NumericalHi _ _ ->
            ""


type Error
    = Critical
    | Normal


type Result error value
    = Ok value
    | Err error


signUp : String -> String -> Result Error String
signUp email ageStr =
    case String.toInt ageStr of
        Nothing ->
            Err Critical

        Just age ->
            let
                isValidEmail =
                    False
            in
            if age < 13 then
                Err Normal

            else if isValidEmail then
                Ok "Your account has been created successfully!"

            else
                Err Normal


type MyList a
    = Empty
    | Node a (MyList a)


list : MyList number
list =
    Node 1 Empty |> Node 12 |> Node 15 |> Node 21 |> Node 33


sum : MyList Int -> Int
sum myList =
    case myList of
        Empty ->
            0

        Node intValue remainingNodes ->
            intValue + sum remainingNodes


main : Html.Html msg
main =
    "low"
        |> (computeTime 3 2
                |> computeSpeed 7.67
                |> escapeEarth 11.2
           )
        |> Html.text
