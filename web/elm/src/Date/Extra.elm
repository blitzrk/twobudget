-- import Date.Extra as DateString
module Date.Extra exposing (fromDate, toLocalDate, daysInMonth, add, weekPos)

import Date exposing (Date)
import String
import Time

fromDate : Date -> String
fromDate date =
  date |> split |> format


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
  in  ( Date.year date, month, Date.day date )


format : (Int, Int, Int) -> String
format (y, m, d) =
  [y, m, d]
    |> List.map toString
    |> List.map (String.padLeft 2 '0')
    |> String.join "-"


toLocalDate : String -> Result String Date
toLocalDate datestring =
  Date.fromString <| datestring ++ " 12:00 MDT" -- TODO: Get with JS native func


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


add : (Int, Int, Int) -> String -> Result String String
add (dy, dm, dd) datestring =
  case toLocalDate datestring of
    Err msg -> Err msg
    Ok date ->
      date
        |> Date.toTime
        |> \t -> t + (24 * Time.hour * toFloat dd)
        |> Date.fromTime
        |> split
        |> \(y, m, d) ->
          let y' = y + dy + ((m + dm - 1) // 12) + (if m + dm < 1 then -1 else 0)
              m' = 1 + ((m + dm - 1) % 12)
              d' =
                case (y', m', 15) |> format |> toLocalDate of
                  Err msg -> Debug.crash msg
                  Ok dt -> min d (daysInMonth dt)
              x = Debug.log "add" [datestring, format (y', m', d')]
          in  Ok <| format (y', m', d')


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

