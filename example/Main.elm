module Main exposing (main)

import Alert
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { alerts : Alert.State }


type Msg
    = ShowSuccessAlert
    | ShowWarningAlert
    | ShowErrorAlert
    | AlertMsg Alert.Msg


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ShowSuccessAlert, class "btn btn-success" ] [ text "Hit me! :D" ]
        , button [ onClick ShowWarningAlert, class "btn btn-warning" ] [ text "Hit me! :|" ]
        , button [ onClick ShowErrorAlert, class "btn btn-danger" ] [ text "Hit me! :(" ]
        , Html.map AlertMsg (Alert.view model.alerts)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowSuccessAlert ->
            let
                ( alerts, cmd ) =
                    Alert.success "Hello, nice to meet you!" model.alerts
            in
            ( { model | alerts = alerts }, Cmd.map AlertMsg cmd )

        ShowWarningAlert ->
            let
                ( alerts, cmd ) =
                    Alert.warning "Be Careful!" model.alerts
            in
            ( { model | alerts = alerts }, Cmd.map AlertMsg cmd )

        ShowErrorAlert ->
            let
                ( alerts, cmd ) =
                    Alert.error "Oh, something goes wrong" model.alerts
            in
            ( { model | alerts = alerts }, Cmd.map AlertMsg cmd )

        AlertMsg subMsg ->
            let
                ( alerts, cmd ) =
                    Alert.update subMsg model.alerts
            in
            ( { model | alerts = alerts }, Cmd.map AlertMsg cmd )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { alerts = Alert.initState }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
