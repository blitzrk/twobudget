module Float.Extra exposing (toFixed, toDollar)

import String


toFixed : Int -> Float -> String
toFixed n num =
    let
        whole =
            truncate num

        part =
            (num - toFloat whole) * 10 ^ (toFloat n) |> abs |> round
    in
        if n > 0 then
            toString whole ++ "." ++ (part |> toString |> String.padLeft n '0')
        else
            toString whole


toDollar : Float -> String
toDollar num =
    if num < 0 then
        "-$" ++ (toFixed 2) (abs num)
    else
        "$" ++ (toFixed 2) num
