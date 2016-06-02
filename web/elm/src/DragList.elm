module DragList exposing (Model, Msg, init, update, subscriptions, view, toList)

import Debug
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Mouse exposing (Position)



-- MODEL


type alias Model model msg =
    { drag : Maybe (Drag model)
    , items : (List (Item model), List (Item model))
    , initItem : model
    , updateItem : msg -> model -> model
    , viewItem : Item model -> Html (Msg model msg)
    }


type alias Drag model =
    { item : Item model
    , pos : Position
    }


type alias Item model =
    { index : Int
    , value : model
    }


init : a -> (b -> a -> a) -> (a -> Html b) -> ( Model a b, Cmd msg, Cmd (Msg a b) )
init initItem updateItem viewItem =
  ( Model 
      Nothing
      ([Item 0 initItem], [])
      initItem
      updateItem
      (\{index,value} -> App.map (Value index) (viewItem value))
  , Cmd.none
  , Cmd.none
  )



-- API


toList : Model a b -> List a
toList {drag, items} =
  let (left, right) = items
  in (case drag of
    Nothing -> left ++ right
    Just {item} -> left ++ (item :: right))
  |> List.map (\{value} -> value)



-- UPDATE


type Msg model msg
    = DragStart (Item model) Position
    | DragAt Position
    | DragEnd Position
    | Over Int
    | Value Int msg


--update : Msg a b -> Model a b -> ( Model a b, Cmd msg, Cmd (Msg a b) )
update msg model =
  let (left, right) = model.items
      updateItem i msg = (\({index, value} as item) ->
        if index /= i then (item, Cmd.none, Cmd.none)
        else
          let (v, cmd, vCmd) = model.updateItem msg value
          in  (Item i v, cmd, Cmd.map (Value i) vCmd))

  in case msg of
    Value i msg ->
      let lefts = List.map (updateItem i msg) left
          left' = List.map (\(a,_,_) -> a) lefts
          rights = List.map (updateItem i msg) right
          right' = List.map (\(a,_,_) -> a) rights
          cmd = List.foldl (\(_,c,_) cs -> Cmd.batch [c, cs]) Cmd.none
          iCmd = List.foldl (\(_,_,c) cs -> Cmd.batch [c, cs]) Cmd.none
      in  ( { model | left = left', right = right' }, cmd, iCmd )
    
    _ ->
      ( updateHelp msg model, Cmd.none, Cmd.none )


updateHelp : Msg a b -> Model a b -> Model a b
updateHelp msg ({drag, items} as model) =
  case msg of
    DragStart ({index} as item) xy ->
      { model |
        drag = Just (Drag item xy),
        items = items |> split index
      }

    DragAt xy ->
      { model |
        drag = Maybe.map (\{item} -> Drag item xy) drag
      }

    DragEnd _ ->
      case drag of
        Nothing -> model
        Just {item} ->
          { model |
            drag = Nothing,
            items = items |> join item |> order
          }

    Over i ->
      { model |
        items = items |> partition i
      }
    
    _ ->
      Debug.crash <| "Error: cannot handle message: " ++ toString msg


split : Int -> (List (Item a), List (Item a)) -> (List (Item a), List (Item a))
split i (left, right) =
  left ++ right
    |> List.filter (\{index} -> index /= i)
    |> List.foldr (
       \item (l, r) ->
         if item.index < i
           then (item :: l, r)
           else (l, item :: r) )
       ([], [])


join : (Item a) -> (List (Item a), List (Item a)) -> (List (Item a), List (Item a))
join mid (left, right) =
  ( left ++ (mid :: right), [] )


partition : Int -> (List (Item a), List (Item a)) -> (List (Item a), List (Item a))
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


order : (List (Item a), List (Item a)) -> (List (Item a), List (Item a))
order (left, right) =
  case right of
    [] -> ( left |> List.indexedMap (\i item -> { item | index = i }), [] )
    _ -> (left, right)


-- SUBSCRIPTIONS


subscriptions : Model a b -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


(=>) = (,)


view : Model a b -> Html Msg
view {drag, items, viewItem} =
  let
    (left, right) =
      items

    realPosition =
      case drag of
        Nothing -> Position 0 0
        Just {pos} -> Position (pos.x - 15) (pos.y - 15)

    defaultStyle =
      [ "background-color" => "limegreen"
      , "width" => "650px"
      , "height" => "50px"
      , "color" => "white"
      , "display" => "flex"
      ]

    element floating item =
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
          [ viewItem item ]
        ]

    toElement item =
      case drag of
        Nothing -> element False item
        Just _ ->
          div [ style ["position" => "relative"] ]
            [ div
              [ onMouseOver (Over item.index)
              , style
                [ "width" => "650px"
                , "height" => "50px"
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
      (List.map toElement left) ++ (case drag of
        Nothing -> List.map toElement right
        Just _ -> empty :: (List.map toElement right))

  in
    div [ style [] ] (list ++ floater)


px : Int -> String
px number =
  toString number ++ "px"


onMouseDown : (Item model) -> Attribute (Msg model msg)
onMouseDown item =
  onWithOptions
    "mousedown"
    (Html.Events.Options True True)
    (Json.map (DragStart item) Mouse.position)
