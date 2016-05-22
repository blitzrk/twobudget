module BudgetView exposing (Model, Msg, init, update, subscriptions, view)

import Budget

import Date exposing (Date)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onBlur)
import Task
import WebSocket
import Window



-- MODEL

type alias Month = (Date.Month, Int)


type alias Model =
  { focus : Int
  , cache : List (Month, Budget.Model)
  , width : Int
  , addr  : String
  }


type Msg
  = Init Date
  | Focus Int
  | Resize Window.Size
  | Budget Month Budget.Msg



-- INIT

init : String -> ( Model, Cmd Msg )
init addr =
  ( Model 0 [] 0 addr
  , Cmd.batch
    [ Task.perform Debug.crash Resize Window.size
    , Task.perform Debug.crash Init Date.now
    ]
  )



-- UPDATE

update : Msg -> Model -> ( Model, Cmd msg, Cmd Msg )
update msg model =
  let only model = ( model, Cmd.none, Cmd.none )
  in case msg of
    Init date ->
      let month = (Date.month date, Date.year date)
      in  only { model | cache = [ (month, Budget.init month 500) ] }
    Focus focus    -> only { model | focus = focus }
    Resize {width} -> only { model | width = width }
    Budget m msg ->
      case List.filter ((==) m << fst) model.cache of
        (_, b) :: [] ->
          let ( b', cmd, bCmd ) = Budget.update msg b
          in  ( { model |
                  cache = List.map
                    (\(m',b) -> if m == m' then (m',b') else (m',b))
                    model.cache
                }
              , cmd
              , bCmd |> Cmd.map (Budget m)
              )
        _ -> ( model, Cmd.none, Cmd.none )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes Resize
    ]



-- VIEW

view : Model -> Html Msg
view model =
  let
    a = 1
  in
    div [] <|
      List.map (\(m,b) -> App.map (Budget m) (Budget.view b)) model.cache

