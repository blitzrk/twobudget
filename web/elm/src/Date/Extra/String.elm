module Date.Extra.String exposing (fromDate, toLocalDate, add)

import Date exposing (Date)
import Date.Extra
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


approxOffset =
  let epoch = Date.fromTime 0
      sign = if Date.year epoch < 1970 then "-" else "+"
      hours = (if Date.year epoch < 1970
               then if Date.minute epoch > 0
                       then 23 - Date.hour epoch
                       else 24 - Date.hour epoch
               else Date.hour epoch)
              |> toString
              |> String.padLeft 2 '0'
      minutes = Date.minute epoch
                  |> toString
                  |> String.padLeft 2 '0'
  in "GMT" ++ sign ++ hours ++ minutes


toLocalDate : String -> Result String Date
toLocalDate datestring =
  Date.fromString <| datestring ++ " 12:00 " ++ approxOffset


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
                  Ok dt -> min d (Date.Extra.daysInMonth dt)
          in  Ok <| format (y', m', d')
