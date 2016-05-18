module TwoBudget exposing (..)

import Budget exposing (Budget)
import Date
import Debug
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task
import Transaction exposing (Transaction)
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
  , budget      : Budget
  , transaction : Transaction
  }


init : {name : String, jwt : String} -> ( Model, Cmd Msg )
init user =
  let
    ws = wsAddr user.name user.jwt
    (transaction, tcmd) = Transaction.init ws
    default =
      -- { user = Nothing
      { user = user
      , state = Overview
      , budget = Budget.init 1 ws
      , transaction = transaction
      }
  in
    (default, Cmd.batch [ Cmd.map Transact tcmd ])



-- UPDATE


type Msg
  = Transact Transaction.Msg
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

      SyncFrom jsonForm ->
        -- decode json, determine which fields to update, modify model
        (model, Cmd.none)



-- SUBSCRIPTIONS

wsAddr : String -> String -> String
wsAddr username jwt =
  "ws://home.krieg.io/" ++ username ++ "?token=" ++ jwt


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none -- WebSocket.listen (wsAddr model.user.name model.user.jwt) SyncFrom



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  main' []
    [ App.map Transact (Transaction.view model.transaction)
    ]
