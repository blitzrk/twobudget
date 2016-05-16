module Budget exposing (..)


type alias Item =
  { itemName   : String
  , prevBudget : Float
  , currBudget : Float
  , currSpent  : Float
  , currRemain : Float
  }


type alias Month = List Item


