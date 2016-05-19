module TwoBudget exposing (..)

import BudgetView
import Transaction

import Date
import Debug
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task
import WebSocket



main =
  App.programWithFlags
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


type alias Model =
  { user        : { name : String, jwt : String }
  , state       : TabView
  , budgetView  : BudgetView.Model
  , transaction : Transaction.Model
  }


init : {name : String, jwt : String} -> ( Model, Cmd Msg )
init user =
  let
    ws = wsAddr user.name user.jwt
    (transaction, tcmd) = Transaction.init ws
    (budgetView, bvcmd) = BudgetView.init ws
    default =
      { user = user
      , state = Budget
      , budgetView = budgetView
      , transaction = transaction
      }
  in
    (default, Cmd.batch
                [ Cmd.map Transact tcmd
                , Cmd.map BudgetView bvcmd
                ])



-- UPDATE


type Msg
  = Transact Transaction.Msg
  | BudgetView BudgetView.Msg
  | SyncFrom String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let {name, jwt} = model.user
  in case msg of
      Transact msg ->
        let ( transaction', cmd', extra ) = Transaction.update msg model.transaction
        in case extra of
          Nothing -> ( {model | transaction = transaction'}, cmd' )
          Just cmd ->
            ( {model | transaction = transaction'}
            , Cmd.batch [cmd', Cmd.map Transact cmd] )

      BudgetView msg ->
        let ( budgetView', cmd', extra ) = BudgetView.update msg model.budgetView
        in case extra of
          Nothing -> ( {model | budgetView = budgetView'}, cmd' )
          Just cmd ->
            ( {model | budgetView = budgetView'}
            , Cmd.batch [cmd', Cmd.map BudgetView cmd] )

      SyncFrom jsonForm ->
        -- decode json, determine which fields to update, modify model
        (model, Cmd.none)



-- SUBSCRIPTIONS

wsAddr : String -> String -> String
wsAddr username jwt =
  "ws://home.krieg.io/" ++ username ++ "?token=" ++ jwt


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map BudgetView (BudgetView.subscriptions model.budgetView)
    --, WebSocket.listen (wsAddr model.user.name model.user.jwt) SyncFrom
    ]



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  main' []
    [ App.map BudgetView (BudgetView.view model.budgetView)
    , App.map Transact (Transaction.view model.transaction)
    ]
