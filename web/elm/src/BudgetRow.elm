module BudgetRow exposing (Model, Msg, init, update, view)

import Debug
import Float.Extra as Float
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import String
import Task


-- MODEL


type alias Model =
  { name : String
  , amnt : String
  , spnt : String
  , left : String
  }


type Msg
  = InputName String
  | InputAmnt String
  | InputSpnt String
  | Update ()
  | Normalize


init : Model
init =
  Model "" "" "" ""



-- API


setSpent : Float -> Model -> Model
setSpent num model =
  { model | spnt = num |> toString |> norm }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg, Cmd Msg )
update msg model =
  let send msg val =
        Task.perform Debug.crash msg (Task.succeed val)

      updateRemain item =
        case String.toFloat item.amnt of
          Err msg -> ""
          Ok amnt -> if amnt < 0 then "" else
            case String.toFloat item.spnt of
              Err msg -> Float.toDollar amnt
              Ok spnt -> if spnt < 0 then "" else Float.toDollar (amnt - spnt)
  
  in case msg of
    InputName name ->
      ( { model | name = String.trim name }, Cmd.none, Cmd.none )
    InputAmnt amnt ->
      ( { model | amnt = String.trim amnt }, Cmd.none, send Update () )
    InputSpnt spnt ->
      ( { model | spnt = String.trim spnt}, Cmd.none, send Update () )
    Update () ->
      ( { model | left = updateRemain model }, Cmd.none, Cmd.none )
    Normalize ->
      ( { model | amnt = norm model.amnt, spnt = norm model.spnt }, Cmd.none, Cmd.none )


norm : String -> String
norm s =
  case String.toFloat s of
    Err _ -> s
    Ok num -> num |> Float.toFixed 2

-- VIEW

(=>) = (,)


view : Model -> Html Msg
view {name, amnt, spnt, left} =
  let
    default = ["flex" => "1", "min-width" => "75px"]
  in
    div [style ["display" => "flex", "width" => "calc(100% - 10px)"]]
      [ input [style default, onInput InputName, value name] []
      , input [type' "number", style default, onInput InputAmnt, onBlur Normalize, value amnt] []
      , input [type' "number", style default, onInput InputSpnt, onBlur Normalize, value spnt] []
      , input [style default, disabled True, value left] []
      --, button [onClick (Remove i), style ["width" => "25px"]] [text "X"]
      ]

