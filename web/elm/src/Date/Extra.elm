module Date.Extra exposing (daysInMonth, intDayOfWeek)

import Date exposing (Date)

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


intDayOfWeek : Date -> Int
intDayOfWeek date =
  case Date.dayOfWeek date of
    Date.Sun -> 0
    Date.Mon -> 1
    Date.Tue -> 2
    Date.Wed -> 3
    Date.Thu -> 4
    Date.Fri -> 5
    Date.Sat -> 6

