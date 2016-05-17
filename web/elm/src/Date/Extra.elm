module Date.Extra exposing (split, toString, weekPos, daysInMonth)

import Date exposing (Date)
import String

split : Date -> (Int, Int, Int)
split date =
  let month = case Date.month date of
    Date.Jan -> 1
    Date.Feb -> 2
    Date.Mar -> 3
    Date.Apr -> 4
    Date.May -> 5
    Date.Jun -> 6
    Date.Jul -> 7
    Date.Aug -> 8
    Date.Sep -> 9
    Date.Oct -> 10
    Date.Nov -> 11
    Date.Dec -> 12
  in ( Date.year date, month, Date.day date )


toString : (Int, Int, Int) -> String
toString (y, m, d) =
  [y, m, d]
    |> List.map Basics.toString
    |> List.map (\s -> if String.length s < 2 then "0" ++ s else s)
    |> String.join "-"


weekPos : Date -> Int
weekPos date =
  case Date.dayOfWeek date of
    Date.Sun -> 0
    Date.Mon -> 1
    Date.Tue -> 2
    Date.Wed -> 3
    Date.Thu -> 4
    Date.Fri -> 5
    Date.Sat -> 6


daysInMonth : Date -> Int
daysInMonth date =
  case Date.month date of
    Date.Jan -> 31
    Date.Feb ->
      let year = Date.year date
          leap = (year % 4 == 0) && (year % 100 /= 0) || (year % 400 == 0)
      in  if leap then 29 else 28
    Date.Mar -> 31
    Date.Apr -> 30
    Date.May -> 31
    Date.Jun -> 30
    Date.Jul -> 31
    Date.Aug -> 31
    Date.Sep -> 30
    Date.Oct -> 31
    Date.Nov -> 30
    Date.Dec -> 31

