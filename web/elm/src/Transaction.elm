module Transaction exposing (Transaction, Msg, init, update, view, setHref)

import Date
import Date.Extra as Date2
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onClick)
import String
import Task
import WebSocket



-- MODEL

type alias Transaction =
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


init : String -> Transaction
init addr =
  { amount   = ""
  , payee    = ""
  , category = ""
  , account  = ""
  , date     = "1970-01-01"
  , note     = ""
  , open     = False
  , error    = ""
  , href     = addr
  }



-- UPDATE

validate : Transaction -> (Bool, String)
validate transaction =
  ( True, "" )


update : Msg -> Transaction -> ( Transaction, Cmd msg, Maybe (Cmd Msg) )
update msg transaction =
  case msg of
    Show ->
      ( {transaction | open = True}
      , Cmd.none, Just <| Task.perform Today Today Date.now )
    Hide       -> ( {transaction | open = False}, Cmd.none, Nothing )
    Amount v   -> ( {transaction | amount = v}, Cmd.none, Nothing )
    Payee v    -> ( {transaction | payee = v}, Cmd.none, Nothing )
    Category v -> ( {transaction | category = v}, Cmd.none, Nothing )
    Account v  -> ( {transaction | account = v}, Cmd.none, Nothing )
    Note v     -> ( {transaction | note = v}, Cmd.none, Nothing )
    Date v     -> ( {transaction | date = v}, Cmd.none, Nothing )
    Today d ->
      ( { transaction |
          date = Date2.toString << Date2.split <| d }
      , Cmd.none, Nothing )
    Submit ->
      case validate transaction of
        ( True, json ) ->
          ( init transaction.href
          , WebSocket.send transaction.href json, Nothing )
        ( False, err ) ->
          ( {transaction | error = err}, Cmd.none, Nothing )


setHref : String -> Transaction -> Transaction
setHref href transaction =
  { transaction |
    href = href }


-- VIEW

view : Transaction -> Html Msg
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

