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
    (transaction, tcmd) = Transaction.init
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let {name, jwt} = model.user
  in case msg of
      Transact msg ->
        let ( transaction, cmd ) = Transaction.update msg model.transaction
        in  ( { model | transaction = transaction }
            , Cmd.map Transact cmd
            )

      BudgetView msg ->
        let ( budgetView, cmd ) = BudgetView.update msg model.budgetView
        in  ( { model | budgetView = budgetView }
            , Cmd.map BudgetView cmd
            )



-- SUBSCRIPTIONS

wsAddr : String -> String -> String
wsAddr username jwt =
  "ws://home.krieg.io/" ++ username ++ "?token=" ++ jwt


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map BudgetView (BudgetView.subscriptions model.budgetView)
    ]



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  main' []
    [ App.map BudgetView (BudgetView.view model.budgetView)
    , aside
      [ style ["display" => "flex", "justify-content" => "center"]
      ]
      [ App.map Transact (Transaction.view model.transaction)
      ]
    ]
