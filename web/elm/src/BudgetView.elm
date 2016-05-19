module BudgetView exposing (Model, Msg, init, update, view)

import Budget

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onBlur)
import Task



-- MODEL

type alias Model =
  { focus : Int
  , cache : List (Int, Budget.Model)
  , width : Int
  , addr : String
  }


type Msg
  = Init ()
  | Focus



-- INIT

init : String -> ( Model, Cmd Msg )
init addr =
  ( Model 0 [] 0 addr, Task.perform Debug.crash Init (Task.succeed ()) )



-- UPDATE

update : Msg -> Model -> ( Model, Cmd msg, Maybe (Cmd Msg) )
update msg model =
  ( model, Cmd.none, Nothing )



-- VIEW

view : Model -> Html Msg
view model =
  div [] []
