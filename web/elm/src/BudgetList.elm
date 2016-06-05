module BudgetList exposing (Model, Msg, init, update, view, add, sum)

import BudgetRow
import DragList

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)


-- INIT


type alias Model a =
  DragList.Model BudgetRow.Model BudgetRow.Msg a


type alias ListMsg =
  DragList.Msg BudgetRow.Model BudgetRow.Msg


init : (Model a, Cmd msg, Cmd Msg)
init =
  let (val, cmd, msg) = DragList.init <|
    DragList.Sig BudgetRow.init BudgetRow.update BudgetRow.view
  in (val, cmd, Cmd.map Subupdate msg)



-- API


add : Model a -> Model a
add model =
  model


sum : Model a -> Float
sum model =
  1.0



-- UPDATE


type Msg
  = Subupdate ListMsg


update : Msg -> Model a -> Model a
update msg model =
  model



-- VIEW


view : Model a -> Html Msg
view model =
  div [] [ text "Hello world!" ]
