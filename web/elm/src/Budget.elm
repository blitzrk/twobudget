module Budget exposing (Budget, init)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onBlur)



-- MODEL

type alias Item =
  { itemName   : String
  , prevBudget : Float
  , currBudget : Float
  , currSpent  : Float
  , currRemain : Float
  }


type alias Month = List Item


type alias Budget =
  { monthFocus : Int
  , monthCache : List (Int, Month)
  , width : Int
  , addr : String
  }


type Msg
  = Update



-- INIT

init : Int -> String -> Budget
init width addr =
  Budget 0 [] width addr



-- UPDATE

-- VIEW

view : Budget -> Html Msg
view model =
  div [] []
