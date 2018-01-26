module View.Settings exposing (view)

import Models exposing (..)
import RemoteData exposing (WebData)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Util
import Dict


{-| The entry point of view settings. Currently we only tabulate the
settings. We might do something different later.
-}
view : SettingsDict -> Html DashboardMsg
view settings =
    tabulate settings


{-| Given the current option, all of options to choose from, and each
option's onClick msg, return html.
-}
selectOptionGroup : List SettingValue -> String -> SettingValue -> Html DashboardMsg
selectOptionGroup allVals name currentVal =
    span [ class "settingsOptions" ]
        (List.map
            (\v ->
                let
                    cls =
                        classList
                            [ ( "settingsOption", True )
                            , ( "active", v == currentVal )
                            ]

                    attr =
                        if v == currentVal then
                            [ cls ]
                        else
                            [ cls, onClick (OnSettingsChange name v) ]

                    txt =
                        case v of
                            ValBool b ->
                                toString b

                            ValString s ->
                                toString s

                            ValNumber f ->
                                toString f

                            ValInvalid ->
                                "Invalid"
                in
                    span attr [ text txt ]
            )
            allVals
        )


boolOptionGroup : String -> Bool -> Html DashboardMsg
boolOptionGroup name current =
    selectOptionGroup [ ValBool True, ValBool False ] name (ValBool current)


stringOptionGroup : List String -> String -> String -> Html DashboardMsg
stringOptionGroup options name val =
    selectOptionGroup (List.map ValString options) name (ValString val)


inputOptionGroup : String -> (String -> msg) -> Html msg
inputOptionGroup currentVal msg =
    input [ placeholder (toString currentVal), onInput msg ] []


toValNumber : String -> SettingValue
toValNumber s =
    case String.toFloat s of
        Ok f ->
            ValNumber f

        Err _ ->
            ValInvalid


tabulateSettingItem : ( String, SettingValue ) -> Html DashboardMsg
tabulateSettingItem ( name, val ) =
    let
        valueView : Html DashboardMsg
        valueView =
            case val of
                ValBool b ->
                    boolOptionGroup name b

                ValString s ->
                    inputOptionGroup s (ValString >> OnSettingsChange name)

                ValNumber f ->
                    inputOptionGroup (toString f) (toValNumber >> OnSettingsChange name)

                ValInvalid ->
                    text ("Internal Error on " ++ name)
    in
        tr []
            [ td [] [ text name ]
            , td [] [ valueView ]
            ]


tabulate : SettingsDict -> Html DashboardMsg
tabulate settings =
    let
        rows =
            List.map tabulateSettingItem (Dict.toList settings)
    in
        table []
            (rows
                ++ [ td [] [ span [ class "btn", onClick OnResetSettings ] [ text "reset" ] ]
                   ]
            )
