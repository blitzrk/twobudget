module BudgetView exposing (Model, Msg, init, update, subscriptions, view)

import Budget

import Date exposing (Date)
import Debug
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onBlur)
import Task
import Window



-- MODEL


type alias Month = (Date.Month, Int)


type alias Model =
  { focus : Int
  , cache : List (Month, Budget.Model)
  , width : Int
  , user  : String
  }


type Msg
  = Init Date
  | Focus Int
  | Resize Window.Size
  | Budget Month Budget.Msg



-- INIT

init : String -> ( Model, Cmd Msg )
init user =
  ( Model 0 [] 0 user
  , Cmd.batch
    [ Task.perform Debug.crash Resize Window.size
    , Task.perform Debug.crash Init Date.now
    ]
  )



-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let only model' = ( model', Cmd.none )
  in case msg of
    Init date ->
      let month = (Date.month date, Date.year date)
          (budget, cmd) = Budget.init month 500
      in  ( { model | cache = [ (month, budget) ] }
          , Cmd.map (Budget month) cmd
          )
    Focus focus    -> only { model | focus = focus }
    Resize {width} -> only { model | width = width }
    Budget m msg ->
      let x = Debug.log "msg" msg in
      case List.filter ((==) m << fst) model.cache of
        (_, b) :: [] ->
          let ( b', cmd ) = Budget.update msg b
          in  ( { model |
                  cache = List.map
                    (\(m',b) -> if m == m' then (m',b') else (m',b))
                    model.cache
                }
              , Cmd.map (Budget m) cmd
              )
        _ -> ( model, Cmd.none )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch <|
    [ Window.resizes Resize
    ] ++ (List.map (\(m,b) -> Sub.map (Budget m) (Budget.subscriptions b)) model.cache)



-- VIEW

view : Model -> Html Msg
view model =
  let
    a = 1
  in
    div [] <|
      List.map (\(m,b) -> App.map (Budget m) (Budget.view b)) model.cache

