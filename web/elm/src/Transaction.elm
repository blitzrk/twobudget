module Transaction exposing (Model, Msg, init, update, view)

import Calendar
import Date exposing (Date)
import Date.Extra.String as DateString
import Dict exposing (Dict)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onClick)
import String
import Task
import WebSocket



-- MODEL

type alias Model =
  { form  : Dict String String
  , open  : Bool
  , error : Maybe String
  , calendar : Calendar.Model
  }


type Msg
  = Reset ()
  | Set String String
  | Today Date
  | Show
  | Submit
  | Calendar Calendar.Msg


init : ( Model, Cmd Msg )
init =
  let (cal, cmd) = Calendar.init
  in  ( { form  = Dict.empty
        , open  = False
        , error = Nothing
        , calendar = cal
        }
      , Cmd.batch
        [ Task.perform Debug.crash Today Date.now
        , Cmd.map Calendar cmd
        ]
      )



-- UPDATE

validate : Model -> (Bool, String)
validate transaction =
  ( True, "" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({calendar, form} as model) =
  let
    chain next val =
      ( model
      , Task.perform Debug.crash next (Task.succeed val)
      )
  in 
    case msg of
      Reset () -> let (model, cmd) = init in (model, cmd)
      Set k v  -> { model | form = form |> Dict.insert k v } ! []
      Today d  -> chain (Set "date") (DateString.fromDate d)
      Show     -> { model | open = True } ! []
      Submit ->
        case validate model of
          ( False, err ) -> {model | error = Just err} ! []
          ( True, json ) ->
            let (model', cmd) = init
            in  model ! [ cmd, Task.perform Debug.crash Reset (Task.succeed ()) ]

      Calendar msg ->
        let (calendar', cmd) = Calendar.update msg calendar
        in  { model | calendar = calendar' } ! [Cmd.map Calendar cmd]



-- VIEW

(=>) = (,)


view : Model -> Html Msg
view {form, open, calendar} =
  let
    value' field =
      value <| Maybe.withDefault "" (form |> Dict.get field)

    toLabel field =
      (String.toUpper <| String.left 1 field) ++ (String.dropLeft 1 field) ++ ":"

    row attrs field =
      label [ style ["width" => "100%"] ]
        [ span [ style ["display" => "block"] ] [ text <| toLabel field ]
        , input (attrs ++
          [ style ["width" => "100%"], onInput (Set field), value' field ])
          []
        ]

    tButton =
      if open
         then button [ onClick (Reset ()) ] [ text "Clear" ]
         else button [ onClick Show ] [ text "Add Transaction" ]
  in
    section
      [ style 
        [ "display" => "flex"
        , "flex-direction" => "column"
        , "margin-top" => "30px"
        , "width" => "320px"
        ]
      ]
      [ tButton
      , Html.form
        [ style
          [ "display" => if open then "flex" else "none"
          , "flex-direction" => "column"
          , "align-items" => "stretch"
          , "width" => "100%"
          ]
        ]
        [ "amount"   |> row [ type' "number" ]
        , "payee"    |> row []
        , "category" |> row []
        , "date"     |> row [ type' "date" ]
        , "note"     |> row []
        , button
          [ style ["margin-top" => "20px"], onClick Submit ]
          [ text "Submit" ]
        ]
      --, App.map Calendar (Calendar.view calendar)
      ]

