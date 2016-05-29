module BudgetList exposing (Model, Msg, init, update, view, add, sum)

import DragList

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)


-- INIT


type alias Model =
  DragList BudgetRow BudgetRow.Msg


init : Model
init =
  DragList.init



-- API


add : Model -> Model
add model =
  model


sum : Model -> Float
sum model =
  1.0



-- UPDATE


type Msg
  = Reset


update : Msg -> Model -> Model
update msg model =
  model



-- VIEW


view : Model -> Html Msg
view model =
  div [] [ text "Hello world!" ]
