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
import Transaction exposing (Transaction)
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
      , transaction = Transaction.new (Date.fromTime 0)
      }
  in
    (default, Cmd.none)



-- UPDATE


type Msg
  = ToggleTransaction
  | InitTransaction Date.Date
  | InputTransaction Transaction.Field String
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
                t = Transaction.new date
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
              transaction = Transaction.update field value model.transaction }
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
      if model.transaction.open then
        Transaction.form InputTransaction SubmitTransaction model.transaction
      else
        noElement
    ]


noElement : Html Msg
noElement =
  div [ style [ "display" => "none" ] ] []
