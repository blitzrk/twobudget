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
          case Transaction.isOpen model.transaction of
            False -> (model, Task.perform InitTransaction InitTransaction Date.now)
            True -> ({ model |
                       transaction = Transaction.toggleForm model.transaction}, Cmd.none)

        InitTransaction date ->
          case Transaction.isOpen model.transaction of -- recheck to avoid a race condition
            True  -> (model, Cmd.none)
            False ->
              let
                newModel =
                  { model |
                    date = date,
                    transaction = Transaction.openForm <| Transaction.new date
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
            validation =
              (True, "")
          in
            case validation of
              (True, jsonForm) ->
                ( { model |
                    transaction = Transaction.closeForm model.transaction
                  }, WebSocket.send (wsAddr name jwt) jsonForm )

              (False, msg) ->
                ( { model |
                    transaction = Transaction.setError msg model.transaction
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
  let
    transactionButton =
      if Transaction.isOpen model.transaction
         then button [ onClick ToggleTransaction ] [ text "-" ]
         else button [ onClick ToggleTransaction ] [ text "+" ]

    transaction =
      if Transaction.isOpen model.transaction
         then Transaction.form InputTransaction SubmitTransaction model.transaction
         else noElement
  in
    main' []
      [
        transactionButton,
        transaction
      ]


noElement : Html Msg
noElement =
  div [ style [ "display" => "none" ] ] []
