module Transaction exposing (Model, Msg, init, update, view, setHref)

import Date
import Date.Extra as Date2
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onClick)
import String
import Task
import WebSocket



-- MODEL

type alias Model =
  { amount   : String
  , payee    : String
  , category : String
  , account  : String
  , date     : String
  , note     : String
  , open     : Bool
  , error    : String
  , href     : String
  }


type Msg
  = Amount String
  | Payee String
  | Category String
  | Account String
  | Date String
  | Note String
  | Show
  | Hide
  | Today Date.Date
  | Submit


init : String -> ( Model, Cmd Msg )
init addr =
  ( { amount   = ""
    , payee    = ""
    , category = ""
    , account  = ""
    , date     = "1970-01-01"
    , note     = ""
    , open     = False
    , error    = ""
    , href     = addr
    }
  , Task.perform Today Today Date.now
  )



-- UPDATE

validate : Model -> (Bool, String)
validate transaction =
  ( True, "" )


update : Msg -> Model -> ( Model, Cmd msg, Cmd Msg )
update msg transaction =
  let only = \model -> ( model, Cmd.none, Cmd.none )
  in case msg of
    Show       -> only { transaction | open = True }
    Hide       -> only { transaction | open = False }
    Amount v   -> only { transaction | amount = v }
    Payee v    -> only { transaction | payee = v }
    Category v -> only { transaction | category = v }
    Account v  -> only { transaction | account = v }
    Note v     -> only { transaction | note = v }
    Date v     -> only { transaction | date = v }
    Today d    -> only { transaction | date = Date2.toString << Date2.split <| d }
    Submit ->
      case validate transaction of
        ( False, err ) -> only {transaction | error = err}
        ( True, json ) ->
          ( fst << init <| transaction.href
          , WebSocket.send transaction.href json
          , Cmd.none
          )


setHref : String -> Model -> Model
setHref href transaction =
  { transaction |
    href = href }


-- VIEW

view : Model -> Html Msg
view {amount, payee, category, account, date, note, open} =
  let
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
          , td [] [ input [type' "number", onInput Amount, value amount ] [] ]
          ]
        , tr []
          [ td [] [ label [] [ text "Payee: " ] ]
          , td [] [ input [ onInput Payee, value payee ] [] ]
          ]
        , tr []
          [ td [] [ label [] [ text "Category: " ] ]
          , td [] [ input [ onInput Category, value category ] [] ]
          ]
        , tr []
          [ td [] [ label [] [ text "Account: " ] ]
          , td [] [ input [ onInput Account, value account ] [] ]
          ]
        , tr []
          [ td [] [ label [] [ text "Date: " ] ]
          , td [] [ input [ type' "date", onInput Date, value date ] [] ]
          ]
        , tr []
          [ td [] [ label [] [ text "Note: " ] ]
          , td [] [ input [ onInput Note, value note ] [] ]
          ]
        , tr [] [ button [onClick Submit] [text "Submit"] ]
        ]
      ]

