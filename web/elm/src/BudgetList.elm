module BudgetList exposing (Model, Msg, init, update, subscriptions, view, add, sum)

import BudgetRow
import DragList
import Debug
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import String


-- INIT


type alias Model =
    DragList.Model BudgetRow.Model BudgetRow.Msg


type alias ListMsg =
    DragList.Msg BudgetRow.Model BudgetRow.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmd ) =
            DragList.init
                <| DragList.Sig BudgetRow.init BudgetRow.update BudgetRow.view
    in
        model ! [ Cmd.map Subupdate cmd ]



-- API


add : Model -> Model
add model =
    model |> DragList.append model.struct.init


sum : Model -> Float
sum model =
    model
        |> DragList.toList
        |> List.foldl
            (\row acc ->
                case String.toFloat row.amnt of
                    Err _ ->
                        acc

                    Ok v ->
                        acc + v
            )
            0



-- UPDATE


type Msg
    = Subupdate ListMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Subupdate message ->
            let
                ( model', cmd ) =
                    DragList.update message model
            in
                model' ! [ Cmd.map Subupdate cmd ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Subupdate (DragList.subscriptions model)



-- VIEW


view : Model -> Html Msg
view model =
    App.map Subupdate (DragList.view model)
