module Budget exposing (Model, Msg, init, update, subscriptions, view)

import ReorderBudgetList

import Date
import Float.Extra as Float
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL

type alias Model =
  { title : String
  , start : Float
  , balance : Float
  , items : ReorderBudgetList.Model
  }


type Msg
  = Add
  | Items ReorderBudgetList.Msg


init : (Date.Month, Int) -> Float -> (Model, Cmd msg, Cmd Msg)
init (month, year) budget =
  let (list, cmd) = ReorderBudgetList.init
  in  ( Model (toString month ++ " " ++ toString year) budget budget list
      , Cmd.none
      , Cmd.map Items cmd
      )



-- UPDATE

update : Msg -> Model -> ( Model, Cmd msg, Cmd Msg )
update msg model =
  case msg of
    Add ->
      ( { model | items = ReorderBudgetList.add model.items }
      , Cmd.none
      , Cmd.none
      )
    Items msg ->
      let (items', cmd, blCmd) = ReorderBudgetList.update msg model.items
          balance' = model.start - ReorderBudgetList.sum items'
      in  ( { model | items = items', balance = balance' }
          , cmd
          , Cmd.map Items blCmd
          )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

(=>) = (,)


view : Model -> Html Msg
view {title, start, balance, items} =
  let
    headers =
      div [style ["display" => "flex", "width" => "calc(100% - 10px)", "text-align" => "center"]]
          [ span [style ["flex" => "1"]] [text "Category"]
          , span [style ["flex" => "1"]] [text "Amount"]
          , span [style ["flex" => "1"]] [text "Spent"]
          , span [style ["flex" => "1"]] [text "Remaining"]
          , span [style ["width" => "25px"]] []
          ]
  in
    section [ style [ "display" => "flex"
                    , "flex-direction" => "column"
                    , "align-items" => "center"
                    , "width" => "100%"
                    , "max-width" => "650px"
                    , "margin" => "auto"
                    , "padding" => "15px"
                    , "border" => "1px solid black"
                    ]
            ] <|
    [ span [] [text title]
    , span []
      [ text "Balance: "
      , span [style ["color" => if balance < 0 then "red" else "green"]]
        [ text <| Float.toDollar balance ]
      ]
    , hr [ style ["width" => "100%"] ] []
    , headers
    ] ++ [ App.map Items (ReorderBudgetList.view items) ] ++
    [ br [] []
    , button [onClick Add] [text "Add"]
    ]

