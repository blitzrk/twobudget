module BudgetList exposing (Model, Msg, init, update, view, add, sum)

import Debug
import Float.Extra as Float
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import String
import Task


-- MODEL

type alias Model =
  { total : Float
  , items : List (Int, Item)
  }


type alias Item =
  { name : String
  , amnt : String
  , spnt : String
  , left : String
  }


type Msg
  = Remove Int
  | InputName Int String
  | InputAmnt Int String
  | InputSpnt Int String
  | Update Int
  | Normalize Int


init : Model
init =
  Model 0 [(0, Item "" "" "" "")]



-- API

add : Model -> Model
add model =
  let items' = (0, Item "" "" "" "") :: (List.map (\(i, it) -> (i + 1, it)) model.items)
  in { model | items = items' }


sum : Model -> Float
sum model =
  model.total



-- UPDATE

update : Msg -> Model -> ( Model, Cmd msg, Cmd Msg )
update msg model =
  let alter i fn =
        { model |
          items = model.items |>
            List.map (\(j, it) ->
              if i == j
                then (i, fn it)
                else (j, it))
        }

      send msg val =
        Task.perform Debug.crash msg (Task.succeed val)

      updateRemain item =
        case String.toFloat item.amnt of
          Err msg -> ""
          Ok amnt -> if amnt < 0 then "" else
            case String.toFloat item.spnt of
              Err msg -> Float.toDollar amnt
              Ok spnt -> if spnt < 0 then "" else Float.toDollar (amnt - spnt)

      normalize i =
        let 
          norm s =
            case String.toFloat s of
              Err _ -> s
              Ok num -> num |> Float.toFixed 2
        in
          model.items |> List.map (\(j, it) ->
            if i == j
              then (i, { it | amnt = norm it.amnt, spnt = norm it.spnt })
              else (j, it))
  
  in case msg of
    Remove i ->
      let items' = List.filter (\(j, _) -> i /= j) model.items
      in ( { model | items = items' }, Cmd.none, Cmd.none )
    InputName i name ->
      ( alter i <| \it -> {it | name = String.trim name}, Cmd.none, Cmd.none )
    InputAmnt i amnt ->
      ( alter i <| \it -> {it | amnt = String.trim amnt}, Cmd.none, send Update i )
    InputSpnt i spnt ->
      ( alter i <| \it -> {it | spnt = String.trim spnt}, Cmd.none, send Update i )
    Update i ->
      let model' = alter i <| \it -> { it | left = updateRemain it }
          total' = model.items |> List.foldl (
            \(_, it) acc ->
              case String.toFloat it.amnt of
                Ok amnt -> if amnt > 0 then acc + amnt else acc
                Err msg -> acc) 0
      in ( { model' | total = total' }, Cmd.none, Cmd.none )
    Normalize i ->
      ( { model | items = normalize i }, Cmd.none, Cmd.none )



-- VIEW

(=>) = (,)


view : Model -> List (Html Msg)
view model =
    List.map viewHelp (List.reverse model.items)


viewHelp : (Int, Item) -> Html Msg
viewHelp (i, {name, amnt, spnt, left}) =
  let
    default = ["flex" => "1", "min-width" => "75px"]
  in
    div [style ["display" => "flex", "width" => "calc(100% - 10px)"]]
      [ input [style default, onInput (InputName i), value name] []
      , input [type' "number", style default, onInput (InputAmnt i), onBlur (Normalize i), value amnt] []
      , input [type' "number", style default, onInput (InputSpnt i), onBlur (Normalize i), value spnt] []
      , input [style default, disabled True, value left] []
      , button [onClick (Remove i), style ["width" => "25px"]] [text "X"]
      ]

