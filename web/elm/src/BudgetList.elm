module BudgetList exposing (Model, Msg, init, update, view, add, sum)

import DragList


-- INIT


type alias Model =
  DragList BudgetRow Msg


init : Model
init =
  DragList.init



-- API

-- UPDATE

-- VIEW
