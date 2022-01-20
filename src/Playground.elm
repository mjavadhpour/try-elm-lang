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


main : Html.Html msg
main =
    "low"
        |> (computeTime 3 2
                |> computeSpeed 7.67
                |> escapeEarth 11.2
           )
        |> Html.text
