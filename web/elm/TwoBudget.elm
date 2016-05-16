module TwoBudget exposing (..)

import Budget
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
  App.program
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
  { user            : Maybe { name : String, jwt : String }
  , date            : Date.Date
  , state           : TabView
  , monthFocus      : Int
  , monthCache      : List (Int, Budget.Month)
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
      , transaction = Transaction.init "" -- set websocket addr on user login
      }
  in
    (default, Cmd.none)



-- UPDATE


type Msg
  = Transact Transaction.Msg
  | SyncFrom String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model.user of
    Nothing ->
      (model, Cmd.none)
    Just {name, jwt} ->
      case msg of
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
  main' []
    [ App.map Transact (Transaction.view model.transaction)
    ]
