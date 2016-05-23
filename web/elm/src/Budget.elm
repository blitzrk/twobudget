module Budget exposing (Model, Msg, init, update, subscriptions, view)

import Date
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import Result
import String
import Task


-- MODEL

type alias Model =
  { title : String
  , start : Float
  , balance : Float
  , items : List (Int, Item)
  }


type alias Item =
  { name : String
  , amnt : String
  , spnt : String
  , left : String
  }


type Msg
  = Add
  | Remove Int
  | InputName Int String
  | InputAmnt Int String
  | InputSpnt Int String
  | Update Int
  | Normalize Int


init : (Date.Month, Int) -> Float -> Model
init (month, year) budget =
  Model
    (toString month ++ " " ++ toString year)
    budget
    budget
    [(0, Item "" "" "" "")]



-- UPDATE

update : Msg -> Model -> ( Model, Cmd msg, Cmd Msg )
update msg model =
  let alter i fn =
        { model |
          items = List.map (\(j, it) ->
                    if i == j
                      then (i, fn it)
                      else (j, it)) model.items }

      send msg val =
        Task.perform Debug.crash msg (Task.succeed val)

      totalBudget =
        List.foldl (
          \(_, it) acc ->
            case String.toFloat it.amnt of
              Ok amnt -> if amnt > 0 then acc + amnt else acc
              Err msg -> acc) 0 model.items
      
      updateRemain item =
        case String.toFloat item.amnt of
          Err msg -> ""
          Ok amnt -> if amnt < 0 then "" else
            case String.toFloat item.spnt of
              Err msg -> toDollar amnt
              Ok spnt -> if spnt < 0 then "" else toDollar (amnt - spnt)

      normalize i =
        let 
          norm s =
            case String.toFloat s of
              Err _ -> s
              Ok num -> floatString num
        in
          model.items |> List.map (\(j, it) ->
            if i == j
              then (i, { it | amnt = norm it.amnt, spnt = norm it.spnt })
              else (j, it))
  
  in case msg of
    Add ->
      let items' = (0, Item "" "" "" "") :: (List.map (\(i, it) -> (i + 1, it)) model.items)
      in ( { model | items = items' }, Cmd.none, Cmd.none )
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
          balance = model.start - totalBudget
      in ( { model' | balance = balance }, Cmd.none, Cmd.none )
    Normalize i ->
      ( { model | items = normalize i }, Cmd.none, Cmd.none )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

(=>) = (,)


floatString : Float -> String
floatString n =
  let whole = truncate n
      part = round <| abs (n - toFloat whole) * 100
  in toString whole ++ "." ++ if part == 0 then "00" else if part < 10 then "0" else "" ++ toString part  


toDollar : Float -> String
toDollar n =
  if n < 0
    then "-$" ++ floatString (abs n)
    else "$" ++ floatString n


toRow : (Int, Item) -> Html Msg
toRow (i, {name, amnt, spnt, left}) =
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


view : Model -> Html Msg
view model =
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
    [ span [] [text model.title]
    , span []
      [ text "Balance: "
      , span [style ["color" => if model.balance < 0 then "red" else "green"]]
        [ text <| toDollar model.balance ]
      ]
    , hr [ style ["width" => "100%"] ] []
    , headers
    ] ++ List.map toRow (List.reverse model.items) ++
    [ br [] []
    , button [onClick Add] [text "Add"]
    ]

