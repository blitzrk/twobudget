module BudgetView exposing (Model, Msg, init, update, subscriptions, view)

import Budget

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onBlur)
import Task
import WebSocket
import Window



-- MODEL

type alias Model =
  { focus : Int
  , cache : List (Int, Budget.Model)
  , width : Int
  , addr : String
  }


type Msg
  = Focus Int
  | Resize Window.Size



-- INIT

init : String -> ( Model, Cmd Msg )
init addr =
  ( Model 0 [] 0 addr, Task.perform Debug.crash Resize Window.size )



-- UPDATE

update : Msg -> Model -> ( Model, Cmd msg, Maybe (Cmd Msg) )
update msg model =
  case msg of
    Focus focus ->
      ( { model | focus = focus }
      , Cmd.none
      , Nothing
      )
    Resize {width, height} ->
      ( { model | width = width }
      , Cmd.none
      , Nothing
      )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes Resize
    ]



-- VIEW

view : Model -> Html Msg
view model =
  div [] [text <| toString model.width]
