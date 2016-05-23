module Float.Extra exposing (toFixed, toDollar)

import String


toFixed : Int -> Float -> String
toFixed n num =
  let whole = truncate num
      part = (num - toFloat whole) * 10^(toFloat n)
  in  if n > 0
         then toString whole ++ "." ++ (part |> zeroPadLeft n)
         else toString whole


zeroPadLeft : Int -> Float -> String
zeroPadLeft n num =
  let str = num |> abs |> round |> toString
      len = String.length str
  in  String.repeat (n - len) "0" ++ str


toDollar : Float -> String
toDollar num =
  if num < 0
    then "-$" ++ (toFixed 2) (abs num)
    else "$" ++ (toFixed 2) num
