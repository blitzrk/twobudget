module Transaction exposing (..)

import Date
import Debug
import Html as H exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onClick)
import String
import WebSocket



type alias Transaction =
  { amount   : String
  , payee    : String
  , category : String
  , account  : String
  , date     : Date.Date
  , note     : String
  , open     : Bool
  , error    : String
  }


type Field
  = Amount
  | Payee
  | Category
  | Account
  | Date
  | Note


update : Field -> String -> Transaction -> Transaction
update field value transaction =
  case field of
    Amount   -> {transaction | amount = value}
    Payee    -> {transaction | payee = value}
    Category -> {transaction | category = value}
    Account  -> {transaction | account = value}
    Note     -> {transaction | note = value}
    Date ->
      case Result.toMaybe <| Date.fromString value of
        Nothing   -> transaction
        Just date -> {transaction | date = date} -- Remember to validate date before syncing!


new : Date.Date -> Transaction
new date =
  { amount   = ""
  , payee    = ""
  , category = ""
  , account  = ""
  , date     = date
  , note     = ""
  , open     = False
  , error    = ""
  }



dateString : Date.Date -> String
dateString date =
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
  in
  [ Date.year date, month, Date.day date + 1 ]
    |> List.map toString
    |> List.map (\s -> if String.length s < 2 then "0" ++ s else s)
    |> String.join "-"


form : (Field -> String -> msg) -> msg -> Transaction -> Html msg
form tagInput tagSubmit {amount, payee, category, account, date, note} =
  let
    updateInput field = onInput <| tagInput field
  in
    H.form []
      [ tr []
        [ td [] [ label [] [ text "Amount: " ] ]
        , td [] [ input [type' "number", updateInput Amount, value amount] [] ]
        ]
      , tr []
        [ td [] [ label [] [ text "Payee: " ] ]
        , td [] [ input [updateInput Payee, value payee] [] ]
        ]
      , tr []
        [ td [] [ label [] [ text "Category: " ] ]
        , td [] [ input [updateInput Category, value category] [] ]
        ]
      , tr []
        [ td [] [ label [] [ text "Account: " ] ]
        , td [] [ input [updateInput Account, value account] [] ]
        ]
      , tr []
        [ td [] [ label [] [ text "Date: " ] ]
        , td [] [ input [type' "date", updateInput Date, value <| dateString date] [] ]
        ]
      , tr []
        [ td [] [ label [] [ text "Note: " ] ]
        , td [] [ input [updateInput Note, value note] [] ]
        ]
      , tr [] [ button [onClick tagSubmit] [text "Submit"] ]
      ]


openForm : Transaction -> Transaction
openForm transaction =
  { transaction | open = True }


closeForm : Transaction -> Transaction
closeForm transaction =
  { transaction | open = False }


toggleForm : Transaction -> Transaction
toggleForm transaction =
  { transaction | open = not transaction.open }


isOpen : Transaction -> Bool
isOpen transaction =
  transaction.open


setError : String -> Transaction -> Transaction
setError msg transaction =
  { transaction | error = msg }

