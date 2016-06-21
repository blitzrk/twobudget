module Transaction exposing (Model, Msg, init, update, view)

import Calendar
import Date exposing (Date)
import Date.Extra.String as DateString
import Dict exposing (Dict)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onClick, onFocus)
import String
import Task


-- MODEL


type alias Model =
    { form : Dict String String
    , open : Bool
    , user : String
    , error : Maybe String
    , showCal : Bool
    , calendar : Calendar.Model
    }


type Msg
    = Reset ()
    | Set String String
    | Show
    | ShowCalendar Bool
    | Submit
    | Calendar Calendar.Msg


init : String -> ( Model, Cmd Msg )
init user =
    let
        ( cal, cmd ) =
            Calendar.init
    in
        { form = Dict.empty
        , open = False
        , user = user
        , error = Nothing
        , showCal = False
        , calendar = cal
        }
            ! [ Cmd.map Calendar cmd
              ]



-- UPDATE
-- Possibly change to (List String) for multiple errs


validate : Model -> Result String ()
validate transaction =
    Ok ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ calendar, form, user } as model) =
    let
        chain next val =
            model ! [ Task.perform Debug.crash next (Task.succeed val) ]
    in
        case msg of
            Reset () ->
                let
                    ( model, cmd ) =
                        init user
                in
                    ( model, cmd )

            Set k v ->
                { model | form = form |> Dict.insert k v } ! []

            Show ->
                { model
                    | open = True
                    , form =
                        form
                            |> Dict.insert "date"
                                (Calendar.string calendar)
                }
                    ! []

            ShowCalendar bool ->
                { model
                    | showCal = bool
                }
                    ! []

            Submit ->
                case validate model of
                    Err err ->
                        { model | error = Just err } ! []

                    Ok json ->
                        model ! [ Task.perform Debug.crash Reset (Task.succeed ()) ]

            Calendar msg ->
                let
                    ( calendar', cmd, datestring ) =
                        Calendar.update msg calendar
                in
                    { model
                        | calendar = calendar'
                        , form =
                            form
                                |> Dict.insert "date"
                                    datestring
                    }
                        ! [ Cmd.map Calendar cmd ]



-- VIEW


(=>) =
    (,)


view : Model -> Html Msg
view { form, open, showCal, calendar } =
    let
        value' field =
            value <| Maybe.withDefault "" (form |> Dict.get field)

        toLabel field =
            (String.toUpper <| String.left 1 field) ++ (String.dropLeft 1 field) ++ ":"

        row attrs field =
            label [ style [ "width" => "100%" ] ]
                [ span [ style [ "display" => "block" ] ] [ text <| toLabel field ]
                , input
                    (attrs
                        ++ [ style [ "width" => "100%" ], onInput (Set field), value' field ]
                    )
                    []
                ]

        tButton =
            if open then
                button [ onClick (Reset ()) ] [ text "Clear" ]
            else
                button [ onClick Show ] [ text "Add Transaction" ]
    in
        section
            [ style
                [ "display" => "flex"
                , "flex-direction" => "column"
                , "margin-top" => "30px"
                , "width" => "320px"
                , "padding" => "0 15px"
                ]
            ]
            [ tButton
            , Html.form
                [ style
                    [ "display"
                        => if open then
                            "flex"
                           else
                            "none"
                    , "flex-direction" => "column"
                    , "align-items" => "stretch"
                    , "width" => "100%"
                    ]
                ]
                [ "amount" |> row [ type' "number", onFocus (ShowCalendar False) ]
                , "payee" |> row [ onFocus (ShowCalendar False) ]
                , "category" |> row [ onFocus (ShowCalendar False) ]
                , "date"
                    |> row
                        [ type' "date"
                        , onFocus (ShowCalendar True)
                        ]
                , if showCal then
                    App.map Calendar (Calendar.view calendar)
                  else
                    div [] []
                , "note" |> row [ onFocus (ShowCalendar False) ]
                , button [ style [ "margin-top" => "20px" ], onClick Submit ]
                    [ text "Submit" ]
                ]
            ]
