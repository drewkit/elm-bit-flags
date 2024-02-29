module Helpers exposing (..)

import Bitwise


sanitizeFlag : String -> String
sanitizeFlag flag =
    flag
        |> String.trim
        |> String.toLower


transformEmptyFlagStringToNothingVal : Maybe String -> Maybe String
transformEmptyFlagStringToNothingVal maybeFlag =
    case maybeFlag of
        Just flag ->
            if String.isEmpty flag then
                Nothing

            else
                Just flag

        _ ->
            Nothing


flagEnabled : Int -> Int -> Bool
flagEnabled flag register =
    Bitwise.and flag register == flag


findFirstNothing : List ( Int, Maybe String ) -> Maybe Int
findFirstNothing list =
    case list of
        ( index, maybeVal ) :: rest ->
            case maybeVal of
                Just _ ->
                    findFirstNothing rest

                Nothing ->
                    Just index

        [] ->
            Nothing


findFlagIndex : List ( Int, Maybe String ) -> String -> Maybe Int
findFlagIndex list target =
    case list of
        ( index, maybeVal ) :: rest ->
            case maybeVal of
                Just val ->
                    if val == target then
                        Just index

                    else
                        findFlagIndex rest target

                Nothing ->
                    findFlagIndex rest target

        [] ->
            Nothing
