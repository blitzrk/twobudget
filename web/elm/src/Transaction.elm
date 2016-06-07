module Transaction exposing (Model, Msg, init, update, view, setHref)

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
  , href  : String
  , calendar : Calendar.Model
  }


type Msg
  = Reset ()
  | Set String String
  | Today Date
  | Show
  | Submit
  | Calendar Calendar.Msg


init : String -> ( Model, Cmd Msg )
init addr =
  let (cal, calCmd) = Calendar.init
  in  ( { form  = Dict.empty
        , open  = False
        , error = Nothing
        , href  = addr
        , calendar = cal
        }
      , Cmd.batch
        [ Task.perform Debug.crash Today Date.now
        , Cmd.map Calendar calCmd
        ]
      )



-- UPDATE

validate : Model -> (Bool, String)
validate transaction =
  ( True, "" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg transaction =
  let only = \model -> ( model, Cmd.none )
      chain msg str =
        ( transaction
        , Task.perform Debug.crash msg (Task.succeed str)
        )
  in case msg of
    Reset () -> let (model, cmd) = init transaction.href in (model, cmd)
    Set k v  -> only { transaction | form = transaction.form |> Dict.insert k v }
    Today d  -> chain (Set "date") (DateString.fromDate d)
    Show     -> only { transaction | open = True }
    Submit ->
      case validate transaction of
        ( False, err ) -> only {transaction | error = Just err}
        ( True, json ) ->
          ( fst << init <| transaction.href
          --, WebSocket.send transaction.href json
          , Task.perform Debug.crash Reset (Task.succeed ())
          )
    Calendar msg ->
      let (calendar', cmd) = Calendar.update msg transaction.calendar
      in  ( { transaction | calendar = calendar' }
          , Cmd.map Calendar cmd
        )


setHref : String -> Model -> Model
setHref href transaction =
  { transaction |
    href = href }



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
      , App.map Calendar (Calendar.view calendar)
      ]

