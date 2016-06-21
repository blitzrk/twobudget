module Calendar exposing (Model, Msg, init, update, view, string)

import Date exposing (Date)
import Date.Extra
import Date.Extra.String as DateString
import Debug
import Html as H exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task


-- MODEL


type alias Model =
    Date


init : ( Model, Cmd Msg )
init =
    let
        epoch =
            Date.fromTime 0
    in
        ( epoch, Task.perform Debug.crash Set Date.now )



-- API


string : Model -> String
string model =
    DateString.fromDate model



-- UPDATE


type Msg
    = Set Date
    | Select Int
    | Next
    | Prev


update : Msg -> Model -> ( Model, Cmd Msg, String )
update msg model =
    let
        incDate delta date =
            case date |> DateString.fromDate |> DateString.add delta of
                Err msg ->
                    Debug.crash msg

                Ok dstr ->
                    case DateString.toLocalDate dstr of
                        Err msg ->
                            Debug.crash msg

                        Ok date' ->
                            date'
    in
        case msg of
            Set date ->
                ( date, Cmd.none, DateString.fromDate model )

            Select i ->
                let
                    d =
                        Date.day model

                    model' =
                        model |> incDate ( 0, 0, i - d )
                in
                    ( model', Cmd.none, DateString.fromDate model' )

            Next ->
                let
                    model' =
                        model |> incDate ( 0, 1, 0 )
                in
                    ( model', Cmd.none, DateString.fromDate model' )

            Prev ->
                let
                    model' =
                        model |> incDate ( 0, -1, 0 )
                in
                    ( model', Cmd.none, DateString.fromDate model' )



-- VIEW


viewMonth : Date -> Html Msg
viewMonth date =
    let
        posOfFirst =
            (Date.Extra.intDayOfWeek date - Date.day date + 1) % 7

        week start current max =
            let
                leftPad =
                    List.map (always <| td [] [ text "" ]) [0..posOfFirst - 1]

                elementStyle i =
                    style
                        <| [ ( "text-align", "center" ) ]
                        ++ if i + start == current then
                            [ ( "color", "white" ), ( "background", "black" ) ]
                           else
                            [ ( "color", "black" ), ( "background", "white" ) ]

                toElement i =
                    td [ onClick (Select <| i + start), elementStyle i ]
                        [ text << toString <| i + start ]
            in
                if start == 1 then
                    (tr [] (leftPad ++ List.map toElement [0..6 - posOfFirst]))
                        :: week (start + 7 - posOfFirst) current max
                else if start <= max then
                    (tr [] (List.map toElement [0..Basics.min 6 (max - start)]))
                        :: week (start + 7) current max
                else
                    []

        weeks =
            week 1 (Date.day date) (Date.Extra.daysInMonth date)
    in
        table [ style [ ( "cursor", "default" ), ( "width", "100%" ) ] ]
            <| [ tr []
                    <| List.map (\day -> td [ style [ ( "text-align", "center" ) ] ] [ text day ])
                        [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]
               ]
            ++ weeks


view : Model -> Html Msg
view model =
    div [ style [ ( "width", "240px" ) ] ]
        [ div [ style [ ( "display", "flex" ), ( "justify-content", "space-between" ), ( "padding", "5px" ) ] ]
            [ button [ onClick Prev ] [ text "<" ]
            , strong []
                [ text << (\d -> toString (Date.month d) ++ " " ++ toString (Date.year d)) <| model ]
            , button [ onClick Next ] [ text ">" ]
            ]
        , viewMonth model
        ]
