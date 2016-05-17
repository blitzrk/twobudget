module Calendar exposing (Calendar, Msg, init, update, view)

import Date exposing (Date)
import Date.Extra as Date2 exposing (weekPos, daysInMonth)
import Debug
import Html as H exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task


-- MODEL

type alias Model = Date


init : ( Model, Cmd Msg )
init =
  let epoch = Date.fromTime 0
  in  ( epoch, Task.perform Set Set Date.now )



-- UPDATE

type Msg
  = Set Date
  | Select Int
  | Next
  | Prev


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let changeMonth delta date =
        date
          |> Date2.split
          |> changeMonthHelp delta
          |> Date2.toString
          |> \d -> d ++ " 12:00"
          |> Date.fromString
      changeMonthHelp delta (y, m, d) =
        let
          (year, month) =
            case m of
              1  -> if delta < 0 then (y - 1, 12) else (y, m + delta)
              12 -> if delta > 0 then (y + 1, 1)  else (y, m + delta)
              _  -> (y, m + delta)
          datestring =
            Date2.toString (year, month, 1) ++ " 12:00"
          day =
            case Date.fromString datestring of
              Ok date -> Basics.min d (daysInMonth date)
              Err msg -> Debug.crash msg
        in
          (year, month, day)
  in case msg of
    Set date ->
      ( date, Cmd.none )
    Select i ->
      let date =
        model
          |> Date2.split
          |> (\(y, m, d) -> (y, m, i))
          |> Date2.toString
          |> \d -> d ++ " 12:00"
          |> Date.fromString
      in case date of
        Ok date -> ( date, Cmd.none)
        Err msg -> ( model, Cmd.none )
    Next ->
      case changeMonth 1 model of
        Ok date -> ( date , Cmd.none)
        Err msg -> ( model , Cmd.none )
    Prev ->
      case changeMonth -1 model of
        Ok date -> ( date , Cmd.none)
        Err msg -> ( model , Cmd.none )



-- VIEW

viewMonth : Date -> Html Msg
viewMonth date =
  let
    posOfFirst = (weekPos date - Date.day date + 1) % 7
    week start current max =
      let leftPad = List.map (always <| td [] [text ""]) [0..posOfFirst-1]
          elementStyle i = style <| [("text-align","center")] ++
                           if i + start == current
                           then [("color","white"), ("background", "black")]
                           else [("color","black"), ("background", "white")]
          toElement i = td [onClick (Select <| i + start), elementStyle i]
                           [text << toString <| i + start]
      in  if start == 1 then
            (tr [] (leftPad ++ List.map toElement [0..6-posOfFirst]))
            :: week (start + 7 - posOfFirst) current max
          else if start <= max then
            (tr [] (List.map toElement [0..Basics.min 6 (max-start)]))
            :: week (start + 7) current max
          else []
    weeks = week 1 (Date.day date) (daysInMonth date)
  in table [style [("cursor","default"), ("width","100%")]] <|
     [ tr [] <|
       List.map (\day -> td [style [("text-align","center")]] [text day])
         ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"]
     ] ++ weeks


view : Model -> Html Msg
view model =
  div [style [("width","240px")]]
    [ div [style [("display","flex"), ("justify-content","space-between"), ("padding","5px")]]
      [ button [onClick Prev] [ text "<" ]
      , strong []
        [ text << (\d -> toString (Date.month d) ++ " " ++ toString (Date.year d)) <| model ]
      , button [onClick Next] [ text ">" ]
      ]
    , viewMonth model
    ]

