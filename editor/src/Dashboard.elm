port module Dashboard exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Api exposing (PollenRockAPI(..))
import View
import Json.Decode as Json


-- MAIN


main : Program Settings DashboardModel DashboardMsg
main =
    Navigation.programWithFlags OnLocationChange
        { init = init
        , view = View.dashboardView
        , update = update
        , subscriptions = subscriptions
        }


initModel : Settings -> Route -> DashboardModel
initModel settings route =
    { route = route
    , fsListDirectory = RemoteData.NotAsked
    , settings = settings
    }


init : Settings -> Location -> ( DashboardModel, Cmd DashboardMsg )
init settings location =
    let
        currentRoute =
            parsePath location.pathname

        model =
            initModel settings currentRoute
    in
        case currentRoute of
            DashboardRoute p ->
                ( model, listDirectory p )

            SettingsRoute ->
                ( model, Cmd.none )

            NotFoundRoute ->
                ( model, Navigation.load "/dashboard" )



-- Updates


settingsUpdate : DashboardMsg -> DashboardModel -> ( DashboardModel, Cmd DashboardMsg )
settingsUpdate msg model =
    let
        settings =
            model.settings
    in
        case msg of
            OnSettingsLineNumberChange ->
                let
                    newSettings =
                        { settings | lineNumbers = not settings.lineNumbers }
                in
                    ( { model | settings = newSettings }, setSettings newSettings )

            OnSettingsLineWrappingChange ->
                let
                    newSettings =
                        { settings | lineWrapping = not settings.lineWrapping }
                in
                    ( { model | settings = newSettings }, setSettings newSettings )

            _ ->
                ( model, Cmd.none )


update : DashboardMsg -> DashboardModel -> ( DashboardModel, Cmd DashboardMsg )
update msg model =
    case msg of
        OnLocationChange location ->
            let
                newRoute =
                    parsePath location.pathname
            in
                ( { model | route = newRoute }, Cmd.none )

        OnDashboardGoBack ->
            ( model, Navigation.back 1 )

        OnDashboardOpenSettings ->
            ( model, Navigation.load "/settings" )

        OnListDirectory data ->
            ( { model | fsListDirectory = data }, Cmd.none )

        _ ->
            settingsUpdate msg model



-- Command


port setSettings : Settings -> Cmd msg


listDirectory : String -> Cmd DashboardMsg
listDirectory srcPath =
    Api.get Api.APIfs Api.fsGetResponseDecoder srcPath
        |> RemoteData.sendRequest
        |> Cmd.map OnListDirectory



-- SUBSCRIPTIONS


subscriptions : DashboardModel -> Sub DashboardMsg
subscriptions model =
    Sub.none
