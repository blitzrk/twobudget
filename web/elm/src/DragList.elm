module DragList exposing (Model, Msg, Sig, init, update, subscriptions, view, toList, append)

import Debug
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseOver, onWithOptions)
import Json.Decode as Json
import Mouse exposing (Position)



-- MODEL


type alias Model model msg =
    { drag : Maybe (Drag model)
    , items : (List (Item model), List (Item model))
    , struct : Sig model msg
    }


type alias Sig model msg =
    { init : model
    , update : msg -> model -> (model, Cmd msg)
    , view : model -> Html msg
    }


type alias Drag model =
    { item : Item model
    , pos : Position
    }


type alias Item model =
    { index : Int
    , value : model
    }


init : Sig a b -> ( Model a b, Cmd (Msg a b) )
init struct =
  ( Model
      Nothing
      ([Item 0 struct.init], [])
      struct
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


append : a -> Model a b -> Model a b
append item ({items} as model) =
  case items of
    (left,[]) -> { model | items = (left ++ [Item (List.length left) item], []) }
    (_,_) -> Debug.log "bad append" model



-- UPDATE


type Msg model msg
    = DragStart (Item model) Position
    | DragAt Position
    | DragEnd Position
    | Over Int
    | Remove Int
    | Value Int msg


update : Msg a b -> Model a b -> ( Model a b, Cmd (Msg a b) )
update msg ({drag, items, struct} as model) =
  case msg of
    DragStart ({index} as item) xy ->
      { model |
        drag = Just (Drag item xy),
        items = items |> split index
      } ! []

    DragAt xy ->
      { model | drag = Maybe.map (\{item} -> Drag item xy) drag } ! []

    DragEnd _ ->
      case drag of
        Nothing -> model ! []
        Just {item} ->
          { model |
            drag = Nothing,
            items = items |> join item |> order
          } ! []

    Over i ->
      { model | items = items |> partition i } ! []

    Remove i ->
      let
        (left, right) = items
        left' = left |> List.filter (\{index} -> index /= i)
        right' = right |> List.filter (\{index} -> index /= i)
      in
        { model | items = (left', right') } ! []

    Value i message ->
      let applyMsg ({index, value} as item) =
            if index /= i then item ! []
            else
              let (v, cmd) = struct.update message value
              in  (Item i v, Cmd.map (Value i) cmd)
          lefts = List.map applyMsg (fst items)
          rights = List.map applyMsg (snd items)
          items' = (List.map fst lefts, List.map fst rights)
          cmds = List.foldl (::) [] (lefts ++ rights |> List.map snd)
      in  { model | items = items' } ! cmds


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


subscriptions : Model a b -> Sub (Msg a b)
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


(=>) = (,)


view : Model a b -> Html (Msg a b)
view {drag, items, struct} =
  let
    (left, right) =
      items

    realPosition =
      case drag of
        Nothing -> Position 0 0
        Just {pos} -> Position (pos.x - 15) (pos.y - 15)

    defaultStyle =
      [ "width" => "650px"
      , "height" => "40px"
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
            , "background-color" => "white"
            ] else
            [ "border-bottom" => "1px solid lightgray"
            , "background-color" => "none"
            ]
        ] <|
        [ div
          [ onMouseDown item
          , style
            [ "height" => "100%"
            , "width" => "25px"
            , "display" => "flex"
            , "justify-content" => "center"
            , "align-items" => "center"
            , "cursor" => "move"
            , "font-weight" => "700"
            ]
          ]
          [ span [ style ["color"=>"darkgray"] ] [ text "☰" ]
          ]
        , div
          [ style
            [ "text-align" => "center"
            , "flex" => "1"
            , "display" => "flex"
            , "justify-content" => "center"
            , "align-items" => "center"
            , "width" => "calc(100% - 50px)"
            ]
          ]
          [ App.map (Value item.index) (struct.view item.value)
          ]
        , div
          [ onClick (Remove item.index)
          , style
            [ "height" => "100%"
            , "width" => "25px"
            , "display" => "flex"
            , "justify-content" => "center"
            , "align-items" => "center"
            , "cursor" => "default"
            , "font-weight" => "700"
            ]
          ]
          [ span [] [text "X"] ]
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
            defaultStyle ++ [ "border-bottom" => "1px solid lightgray" ]
          ] []

    list =
      (List.map toElement left) ++ (case drag of
        Nothing -> List.map toElement right
        Just _ -> empty :: (List.map toElement right))

  in
    div [] (list ++ floater)


px : Int -> String
px number =
  toString number ++ "px"


onMouseDown : Item model -> Attribute (Msg model msg)
onMouseDown item =
  onWithOptions
    "mousedown"
    (Html.Events.Options True True)
    (Json.map (DragStart item) Mouse.position)
