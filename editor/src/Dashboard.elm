port module Dashboard exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Api exposing (PollenRockAPI(..))
import View
import Json.Decode as Json
import Dict


-- MAIN


main : Program JSSettings DashboardModel DashboardMsg
main =
    Navigation.programWithFlags OnLocationChange
        { init = init
        , view = View.dashboardView
        , update = update
        , subscriptions = subscriptions
        }


initModel : JSSettings -> Route -> DashboardModel
initModel settings route =
    { route = route
    , fsListDirectory = RemoteData.NotAsked
    , settings = toSettingsDict settings
    }


init : JSSettings -> Location -> ( DashboardModel, Cmd DashboardMsg )
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

        OnSettingsChange name val ->
            updateSettings model name val

        OnResetSettings ->
            ( model
            , Cmd.batch
                [ resetSettings ()
                , Navigation.reload
                ]
            )


updateSettings : DashboardModel -> String -> SettingValue -> ( DashboardModel, Cmd DashboardMsg )
updateSettings model name val =
    let
        newSettings =
            Dict.insert name val model.settings
    in
        case val of
            ValInvalid ->
                let
                    _ =
                        Debug.log "receive invalid value in settings. Ignore"
                in
                    ( model, Cmd.none )

            ValBool b ->
                ( { model | settings = newSettings }, setBoolSettings ( name, b ) )

            ValString s ->
                ( { model | settings = newSettings }, setStringSettings ( name, s ) )

            ValNumber n ->
                ( { model | settings = newSettings }, setNumberSettings ( name, n ) )



-- Command


port setStringSettings : ( String, String ) -> Cmd msg


port setNumberSettings : ( String, Float ) -> Cmd msg


port setBoolSettings : ( String, Bool ) -> Cmd msg


port resetSettings : () -> Cmd msg


listDirectory : String -> Cmd DashboardMsg
listDirectory srcPath =
    Api.get Api.APIfs Api.fsGetResponseDecoder srcPath
        |> RemoteData.sendRequest
        |> Cmd.map OnListDirectory



-- SUBSCRIPTIONS


subscriptions : DashboardModel -> Sub DashboardMsg
subscriptions model =
    Sub.none
