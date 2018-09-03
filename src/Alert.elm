module Alert
    exposing
        ( State
        , Msg
        , initState
        , update
        , view
        , success
        , warning
        , error
        )

{-| This module implement bootstrap alerts


### As this project uses bootstrap look and feel, you have to use the bootstrap

-- See an example:

    type alias Model =
        { alerts : Alert.State }

    type Msg
        = ShowAlert
        | AlertMsg Alert.Msg

    view : Model -> Html Msg
    view model =
        div []
            [ button [ onClick ShowAlert, class "btn btn-success" ] [ text "Hit me!" ]
            , Html.map AlertMsg (Alert.view model.alerts)
            ]

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ShowAlert ->
                let
                    ( alerts, cmd ) =
                        Alert.success "Hello, nice to meet you!" model.alerts
                in
                    { model | alerts = alerts } ! [ Cmd.map AlertMsg cmd ]

            AlertMsg subMsg ->
                let
                    ( alerts, cmd ) =
                        Alert.update subMsg model.alerts
                in
                    { model | alerts = alerts } ! [ Cmd.map AlertMsg cmd ]

    main : Program Never Model Msg
    main =
        Html.program
            { init = { alerts = Alert.initState } ! []
            , view = view
            , update = update
            , subscriptions = always Sub.none
            }

@docs State
@docs initState
@docs view
@docs Msg
@docs initState
@docs update
@docs view
@docs success
@docs warning
@docs error

-}

import Html exposing (div, span, text, li)
import Html.Keyed exposing (ul)
import Html.Attributes exposing (class, classList, style)
import Task
import Process


{-| This is an opaque type
-}
type State
    = State
        { alerts : List Alert
        , lastId : Int
        }


type alias Alert =
    { type_ : AlertType
    , id : Int
    , message : String
    , status : Status
    }


type AlertType
    = Success
    | Warning
    | Error


type Status
    = Started
    | Animating
    | Leaving


type Id
    = Int


{-| Don't touch!
-}
type Msg
    = OnAlertAdded Int
    | OnAlertStartRemoving Int
    | OnAlertRemoved Int


{-| Function that ueturns the initial state, as we are using opaque types you dun't access to the internal values
-}
initState : State
initState =
    State { alerts = [], lastId = 0 }


{-| View
-}
view : State -> Html.Html Msg
view (State state) =
    let
        alertsView =
            List.map alertView state.alerts
    in
        ul [ alertsContainerStyle ] alertsView


alertView : Alert -> ( String, Html.Html Msg )
alertView alert =
    let
        alertClass =
            alertTypeClass alert.type_
    in
        ( "alerts"
        , li
            [ class ("alert " ++ alertClass)
            , alertStyle (alert.status == Animating)
            ]
            [ span [ alertContentStyle ] [ text alert.message ]
            ]
        )


{-| Success alert ( the green one :o )
-}
success : String -> State -> ( State, Cmd Msg )
success message state =
    genericAlert message Success state


{-| Warning alert ( the yellow one :o )
-}
warning : String -> State -> ( State, Cmd Msg )
warning message state =
    genericAlert message Warning state


{-| Error alert ( the red one :o )
-}
error : String -> State -> ( State, Cmd Msg )
error message state =
    genericAlert message Error state


genericAlert : String -> AlertType -> State -> ( State, Cmd Msg )
genericAlert message alertType (State state) =
    let
        newId =
            state.lastId + 1

        newAlert =
            { type_ = alertType, id = newId, message = message, status = Started }
    in
        State { state | alerts = (state.alerts ++ [ newAlert ]), lastId = newId } ! [ Task.perform (\_ -> OnAlertAdded newId) <| Process.sleep 1 ]


{-| Update function
-}
update : Msg -> State -> ( State, Cmd Msg )
update msg (State state) =
    case msg of
        OnAlertAdded alertId ->
            let
                newAlerts =
                    List.map
                        (\alert ->
                            if alert.id == alertId then
                                { alert | status = Animating }
                            else
                                alert
                        )
                        state.alerts

                newCmds =
                    List.map
                        (\alert ->
                            if alert.id == alertId then
                                Task.perform (\_ -> OnAlertStartRemoving alert.id) <| Process.sleep 5000
                            else
                                Cmd.none
                        )
                        state.alerts
            in
                State { state | alerts = newAlerts } ! newCmds

        OnAlertStartRemoving alertId ->
            let
                newAlerts =
                    List.map
                        (\alert ->
                            if alert.id == alertId then
                                { alert | status = Leaving }
                            else
                                alert
                        )
                        state.alerts

                newCmds =
                    List.map
                        (\alert ->
                            if alert.id == alertId then
                                Task.perform (\_ -> OnAlertRemoved alert.id) <| Process.sleep 600
                            else
                                Cmd.none
                        )
                        state.alerts
            in
                State { state | alerts = newAlerts } ! newCmds

        OnAlertRemoved alertId ->
            let
                newAlerts =
                    List.filter (\alert -> alertId /= alert.id) state.alerts
            in
                State { state | alerts = newAlerts } ! []


alertTypeClass : AlertType -> String
alertTypeClass alertType =
    case alertType of
        Success ->
            "alert-success"

        Warning ->
            "alert-warning"

        Error ->
            "alert-danger"


alertsContainerStyle : Html.Attribute Msg
alertsContainerStyle =
    style
        [ ( "position", "fixed" )
        , ( "width", "100%" )
        , ( "top", "0px" )
        , ( "left", "0px" )
        , ( "padding", "10px" )
        , ( "z-index", "999" )
        , ( "border-radius", "10px" )
        ]


alertStyle : Bool -> Html.Attribute Msg
alertStyle withOpacity =
    let
        opacity =
            if withOpacity then
                "1"
            else
                "0"
    in
        style
            [ ( "width", "100%" )
            , ( "transition", "opacity 0.6s ease-in-out" )
            , ( "opacity", opacity )
            , ( "z-index", "99" )
            , ( "top", "0px" )
            , ( "left", "0px" )
            ]


alertContentStyle : Html.Attribute Msg
alertContentStyle =
    style
        [ ( "padding", "10px" )
        ]
