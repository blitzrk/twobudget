module BudgetList exposing (Model, Msg, init, update, view, add, sum)

import BudgetRow
import DragList

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)


-- INIT


type alias Model =
  DragList.Model BudgetRow.Model BudgetRow.Msg


type alias ListMsg =
  DragList.Msg BudgetRow.Model BudgetRow.Msg


init : (Model, Cmd Msg)
init =
  let (val, cmd) = DragList.init <|
    DragList.Sig BudgetRow.init BudgetRow.update BudgetRow.view
  in (val, Cmd.map Subupdate cmd)



-- API


add : Model -> Model
add model =
  model


sum : Model -> Float
sum model =
  1.0



-- UPDATE


type Msg
  = Subupdate ListMsg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div [] [ text "Hello world!" ]
