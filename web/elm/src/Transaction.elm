module Transaction exposing (Model, Msg, init, update, view, setHref)

import Date exposing (Date)
import Date.Extra as Date2
import Dict exposing (Dict)
import Html exposing (..)
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
  }


type Msg
  = Reset ()
  | Set String String
  | Today Date
  | Show
  | Submit


init : String -> ( Model, Cmd Msg )
init addr =
  ( { form  = Dict.empty
    , open  = False
    , error = Nothing
    , href  = addr
    }
  , Task.perform Debug.crash Today Date.now
  )



-- UPDATE

validate : Model -> (Bool, String)
validate transaction =
  ( True, "" )


update : Msg -> Model -> ( Model, Cmd msg, Cmd Msg )
update msg transaction =
  let only = \model -> ( model, Cmd.none, Cmd.none )
      chain msg str =
        ( transaction
        , Cmd.none
        , Task.perform Debug.crash msg (Task.succeed str)
        )
  in case msg of
    Reset () -> let (model, cmd) = init transaction.href in (model, Cmd.none, cmd)
    Set k v  -> only { transaction | form = transaction.form |> Dict.insert k v }
    Today d  -> chain (Set "date") (Date2.toString <| Date2.split d)
    Show     -> only { transaction | open = True }
    Submit ->
      case validate transaction of
        ( False, err ) -> only {transaction | error = Just err}
        ( True, json ) ->
          ( fst << init <| transaction.href
          , WebSocket.send transaction.href json
          , Task.perform Debug.crash Reset (Task.succeed ())
          )


setHref : String -> Model -> Model
setHref href transaction =
  { transaction |
    href = href }



-- VIEW

(=>) = (,)


view : Model -> Html Msg
view {form, open} =
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
      ]

