module Util exposing (..)

{-| This file contains helper functions.
-}


{-| Construct url path elements. Ignore the empty elements to remove
unnecessary slash in the url (even the trailing slash might be
removed)
-}
concatUrl : List String -> String
concatUrl parts =
    let
        list =
            List.filter (\s -> not (String.isEmpty s)) parts
    in
        String.join "/" list


splitUrl : String -> List String
splitUrl url =
    case String.split "/" url of
        "" :: segments ->
            segments

        segments ->
            segments
