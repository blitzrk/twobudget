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
  | Hide
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
    Hide     -> only { transaction | open = False }
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

view : Model -> Html Msg
view {form, open} =
  let
    value' field = value <| Maybe.withDefault "" (form |> Dict.get field)
    tButton =
      if open
         then button [ onClick Hide ] [ text "-" ]
         else button [ onClick Show ] [ text "+" ]
  in
    section []
      [ tButton
      , br [] []
      , Html.form [ style [("display", if open then "block" else "none")] ]
        [ tr []
          [ td [] [ label [] [ text "Amount: " ] ]
          , td [] [ input [type' "number", onInput (Set "amount"), value' "amount" ] [] ]
          ]
        , tr []
          [ td [] [ label [] [ text "Payee: " ] ]
          , td [] [ input [ onInput (Set "payee"), value' "payee" ] [] ]
          ]
        , tr []
          [ td [] [ label [] [ text "Category: " ] ]
          , td [] [ input [ onInput (Set "category"), value' "category" ] [] ]
          ]
        , tr []
          [ td [] [ label [] [ text "Account: " ] ]
          , td [] [ input [ onInput (Set "account"), value' "account" ] [] ]
          ]
        , tr []
          [ td [] [ label [] [ text "Date: " ] ]
          , td [] [ input [ type' "date", onInput (Set "date"), value' "date" ] [] ]
          ]
        , tr []
          [ td [] [ label [] [ text "Note: " ] ]
          , td [] [ input [ onInput (Set "note"), value' "note" ] [] ]
          ]
        , tr [] [ button [onClick Submit] [text "Submit"] ]
        ]
      ]

