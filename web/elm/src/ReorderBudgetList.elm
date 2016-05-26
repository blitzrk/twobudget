module ReorderBudgetList exposing (Model, Msg, init, update, view, add, sum)

import BudgetList

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Mouse exposing (Position)



-- MODEL


type alias Model =
    { drag : Maybe Drag
    , left : BudgetList.Model
    , right : BudgetList.Model
    }


type alias Item =
    ( Int, BudgetList.Item )


type alias Drag =
    { item : Item
    , pos : Position
    }


init : (Model, Cmd Msg)
init =
  ( Model Nothing BudgetList.init BudgetList.empty, Cmd.none )



-- API

add : Model -> Model
add model =
  { model | left = BudgetList.init `BudgetList.append` model.left }


sum : Model -> Float
sum {left, right} =
  left.total + right.total



-- UPDATE

type Msg
    = DragStart Item Position
    | DragAt Position
    | DragEnd Position
    | Over Int
    | UpdateList BudgetList.Msg


update : Msg -> Model -> ( Model, Cmd msg, Cmd Msg )
update msg ({drag, left, right} as model) =
  case msg of
    UpdateList msg ->
      let (left', cmd1, blCmd1) = BudgetList.update msg left
          (right', cmd2, blCmd2) = BudgetList.update msg right
          cmd = Cmd.batch [cmd1, cmd2]
          blCmd = Cmd.map UpdateList <| Cmd.batch [blCmd1, blCmd2]
      in  ( Model drag left' right', cmd, blCmd )

    _ ->
      ( updateHelp msg model, Cmd.none, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({drag, left, right} as model) =
  case msg of
    DragStart ((i,_) as item) xy ->
      let (left', right') = BudgetList.split i left
      in  Model (Just (Drag item xy)) left' right'

    DragAt xy ->
      Model (Maybe.map (\{item} -> Drag item xy) drag) left right

    DragEnd _ ->
      case drag of
        Nothing -> model
        Just {item} ->
          Model Nothing (BudgetList.join (snd item) (left, right)) BudgetList.empty

    Over i ->
      let (left', right') = BudgetList.partition i left
      in  Model drag left' right'

    _ -> model



{--
split : Int -> (List Item, List Item) -> (List Item, List Item)
split i (left, right) =
  left ++ right
    |> List.filter (\{index} -> index /= i)
    |> List.foldr (
       \item (l, r) ->
         if item.index < i
           then (item :: l, r)
           else (l, item :: r) )
       ([], [])


join : Item -> (List Item, List Item) -> (List Item, List Item)
join mid (left, right) =
  ( left ++ (mid :: right), [] )


partition : Int -> (List Item, List Item) -> (List Item, List Item)
partition i (left, right) =
  let
    op =
      case right of
        [] -> (<)
        {index} :: _ ->
          if i < index
            then (<)
            else (<=)
  in
    left ++ right |> List.foldr (
      \item (l, r) ->
        if item.index `op` i
          then (item :: l, r)
          else (l, item :: r) )
      ([], [])


order : (List Item, List Item) -> (List Item, List Item)
order (left, right) =
  case right of
    [] -> ( left |> List.indexedMap (\i item -> { item | index = i }), [] )
    _ -> (left, right)
--}


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view {drag, left, right} =
  let
    realPosition =
      case drag of
        Nothing -> Position 0 0
        Just {pos} -> Position (pos.x - 15) (pos.y - 15)

    defaultStyle =
      [ "background-color" => "limegreen"
      , "width" => "200px"
      , "color" => "white"
      , "display" => "flex"
      , "height" => "30px"
      ]

    element floating ((i,_) as item) =
      div
        [ style <| defaultStyle ++
          if floating then
            [ "box-shadow" => "0 0 10px black"
            , "position" => "absolute"
            , "left" => px realPosition.x
            , "top" => px realPosition.y
            ] else [ "border-bottom" => "1px solid black" ]
        ] <|
        [ div
          [ onMouseDown item
          , style
            [ "height" => "100%"
            , "width" => "30px"
            , "display" => "flex"
            , "justify-content" => "center"
            , "align-items" => "center"
            , "cursor" => "move"
            ]
          ]
          [ span [] [text "☰"] ]
        , div
          [ style
            [ "text-align" => "center"
            , "flex" => "1"
            , "display" => "flex"
            , "justify-content" => "center"
            , "align-items" => "center"
            ]
          ]
          [ App.map UpdateList (BudgetList.viewItem item) ]
        ]

    toElement ((i,_) as item) =
      case drag of
        Nothing -> element False item
        Just _ ->
          div [ style ["position" => "relative"] ]
            [ div
              [ onMouseOver (Over i)
              , style
                [ "width" => "200px"
                , "height" => "30px"
                , "z-index" => "100"
                , "position" => "absolute"
                , "top" => "0"
                , "left" => "0"
                ]
              ] []
            , element False item
            ]

    floater =
      case drag of
        Nothing -> []
        Just {item} -> [element True item]

    empty =
      div [ style <|
            defaultStyle ++ [ "border-bottom" => "1px solid black" ]
          ] []

    list =
      (List.map toElement left.items) ++ (case drag of
        Nothing -> List.map toElement right.items
        Just _ -> empty :: (List.map toElement right.items))

  in
    div [ style [] ] (list ++ floater)


px : Int -> String
px number =
  toString number ++ "px"


onMouseDown : Item -> Attribute Msg
onMouseDown item =
  onWithOptions
    "mousedown"
    (Html.Events.Options True True)
    (Json.map (DragStart item) Mouse.position)

