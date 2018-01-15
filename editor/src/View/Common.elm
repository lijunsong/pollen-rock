module View.Common exposing (..)

import Models exposing (..)
import Html exposing (Html, program, div, text, a, ul, li, span)
import Html.Attributes exposing (href, class, name, id)
import Util


{-| Calling `breadcrubm rootName "/" "/path1/path2/path3"` will generate

    [(rootName, "/") ("path1" "/path1"), ("path2", "/path1/path2"),
     ("path3", "/path1/path2/path3")]

Note: "" will generate

    [(rootName, "/")]

-}
explodePath : String -> String -> String -> List ( String, String )
explodePath rootName rootPath pathname =
    let
        gen : String -> List ( String, String ) -> List ( String, String )
        gen elm rev_result =
            case List.head rev_result of
                Nothing ->
                    -- this is unreachable
                    [ ( rootName, "/dashboard" ) ]

                Just ( x, lastPath ) ->
                    ( elm, Util.concatUrl [ lastPath, elm ] ) :: rev_result

        elms =
            Util.splitUrl pathname
    in
        List.reverse (List.foldl gen [ ( rootName, "/dashboard" ) ] elms)


breadcrumb : String -> Html msg
breadcrumb pathname =
    let
        paths =
            explodePath "Dashboard" "/dashboard" pathname
    in
        ul [ class "breadcrumb" ]
            (List.map
                (\( name, path ) ->
                    li [] [ a [ href path, class "breadcrumbElement" ] [ text name ] ]
                )
                paths
            )


{-| Show breadcrumb
-}
dashboardHeader : Route -> Html DashboardMsg
dashboardHeader route =
    let
        headLinks : List String -> String -> List (Html DashboardMsg)
        headLinks elms urlPath =
            case elms of
                [] ->
                    []

                hd :: [] ->
                    let
                        newUrl =
                            Util.concatUrl [ urlPath, hd ]
                    in
                        [ a [ href urlPath ] [ text hd ] ]

                hd :: tl ->
                    let
                        newUrl =
                            Util.concatUrl [ urlPath, hd ]
                    in
                        a [ href urlPath ] [ text hd ]
                            :: span [ class "dashboard-header-sep" ] [ text "/" ]
                            :: headLinks tl newUrl
    in
        case route of
            DashboardRoute url ->
                let
                    elms =
                        "root" :: Util.splitUrl url
                in
                    div [] (headLinks elms "/dashboard")

            _ ->
                text ("failed to get route " ++ toString route)


makeHeader : Html msg -> List (Html msg) -> List (Html msg) -> Html msg
makeHeader title left right =
    div [ class "header" ]
        [ div [ class "headerTop" ]
            [ span [ class "title" ] [ title ] ]
        , div [ class "headerBottom" ]
            [ div [ class "headerLeft" ] left
            , div [ class "headerRight" ] right
            ]
        ]
