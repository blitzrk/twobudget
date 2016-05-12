module TwoBudget exposing (..)

import Date
import Debug
import Html as H exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onClick)
import Result
import String
import Task
import WebSocket



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type TabView
  = Accounts
  | Budget
  | Overview


type alias BudgetItem =
  { itemName   : String
  , prevBudget : Float
  , currBudget : Float
  , currSpent  : Float
  , currRemain : Float
  }


type alias BudgetMonth = List BudgetItem


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


type TransactionField
  = Amount
  | Payee
  | Category
  | Account
  | Date
  | Note


type alias Model =
  { user            : Maybe { name : String, jwt : String }
  , date            : Date.Date
  , state           : TabView
  , monthFocus      : Int
  , monthCache      : List (Int, BudgetMonth)
  , transaction     : Transaction
  }
    

init : ( Model, Cmd Msg )
init =
  let
    default =
      -- { user = Nothing
      { user = Just {name="ben", jwt="3"}
      , date = Date.fromTime 0
      , state = Overview
      , monthFocus = -1
      , monthCache = []
      , transaction = newTransaction (Date.fromTime 0)
      }
  in
    (default, Cmd.none)



-- UPDATE


type Msg
  = ToggleTransaction
  | InitTransaction Date.Date
  | InputTransaction TransactionField String
  | SubmitTransaction
  | SyncFrom String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model.user of
    Nothing ->
      (model, Cmd.none)
    Just {name, jwt} ->
      case msg of
        ToggleTransaction ->
          case model.transaction.open of
            False -> (model, Task.perform InitTransaction InitTransaction Date.now)
            True ->
              let transaction = model.transaction
              in ({model | transaction = { transaction | open = False}}, Cmd.none)

        InitTransaction date ->
          case model.transaction.open of -- recheck to avoid a race condition
            True  -> (model, Cmd.none)
            False ->
              let
                t = newTransaction date
                newModel =
                  { model |
                    date = date,
                    transaction = { t | open = True }
                  }
              in
                (newModel, Cmd.none)
        
        InputTransaction field value ->
          let newModel = 
            { model |
              transaction = updateTransaction field value model.transaction }
          in (newModel, Cmd.none)
        
        SubmitTransaction ->
          let
            transaction = model.transaction
            
            validation =
              (True, "")
          in
            case validation of
              (True, jsonForm) ->
                ( { model |
                    transaction = { transaction | open = False }
                  }, WebSocket.send (wsAddr name jwt) jsonForm )
                  
              (False, msg) ->
                ( { model |
                    transaction = { transaction | error = msg }
                  }, Cmd.none )

        SyncFrom jsonForm ->
          -- decode json, determine which fields to update, modify model
          (model, Cmd.none)


updateTransaction : TransactionField -> String -> Transaction -> Transaction
updateTransaction field value transaction =
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


newTransaction : Date.Date -> Transaction
newTransaction date =
  { amount   = ""
  , payee    = ""
  , category = ""
  , account  = ""
  , date     = date
  , note     = ""
  , open     = False
  , error    = ""
  }



-- SUBSCRIPTIONS

wsAddr : String -> String -> String
wsAddr username jwt =
  "ws://127.0.0.1/" ++ username ++ "?token=" ++ jwt


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.user of
    Nothing -> Sub.none
    Just {name, jwt} -> Sub.none -- WebSocket.listen (wsAddr name jwt) SyncFrom



-- VIEW


(=>) = (,)

  
view : Model -> Html Msg
view model =
  div []
    [ div [] [ text "Hello World" ],
      button [ onClick ToggleTransaction ] [ text (if model.transaction.open then "-" else "+") ],
      if model.transaction.open then transactionForm InputTransaction model.transaction else noElement
    ]


noElement : Html Msg
noElement =
  div [ style [ "display" => "none" ] ] []


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


transactionForm : (TransactionField -> String -> Msg) -> Transaction -> Html Msg
transactionForm tagger {amount, payee, category, account, date, note} =
  let
    updateInput field = onInput <| tagger field
  in
    -- TODO: Change to form element
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
      , tr [] [ button [onClick SubmitTransaction] [text "Submit"] ]
      ]



