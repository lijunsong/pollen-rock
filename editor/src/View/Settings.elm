module View.Settings exposing (view)

import Models exposing (..)
import RemoteData exposing (WebData)
import Html exposing (..)
import Html.Attributes exposing (href, class, classList, name, id)
import Html.Events exposing (onClick)
import Util


{-| The entry point of view settings. Currently we only tabulate the
settings. We might do something different later.
-}
view : Settings -> Html DashboardMsg
view settings =
    tabulate settings


optionGroup : opt -> List opt -> List msg -> Html msg
optionGroup current options msgs =
    span [ class "settingsOptions" ]
        (List.map2
            (\option msg ->
                let
                    cls =
                        classList
                            [ ( "settingsOption", True )
                            , ( "active", option == current )
                            ]
                in
                    span [ onClick msg, cls ] [ text (toString option) ]
            )
            options
            msgs
        )


boolOptionGroup : Bool -> msg -> Html msg
boolOptionGroup current msg =
    optionGroup current [ True, False ] [ msg, msg ]


tabulate : Settings -> Html DashboardMsg
tabulate settings =
    table []
        [ tr []
            [ td [] [ text "lineNumbers" ]
            , td [] [ boolOptionGroup settings.lineNumbers OnSettingsLineNumberChange ]
            ]
        , tr []
            [ td [] [ text "lineWrapping" ]
            , td [] [ boolOptionGroup settings.lineWrapping OnSettingsLineWrappingChange ]
            ]
        ]
