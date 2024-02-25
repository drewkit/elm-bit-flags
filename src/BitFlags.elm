module BitFlags exposing (..)

import Array exposing (Array)
import Bitwise
import Set exposing (Set)


type alias BitFlagSettings =
    Array (Maybe String)


transformEmptyToNothing : Maybe String -> Maybe String
transformEmptyToNothing str =
    case str of
        Just s ->
            if String.trim s == "" then
                Nothing

            else
                Just s

        Nothing ->
            Nothing



-- Flag Setting Functions


initSettings : { bitLimit : Int, flags : List String } -> Result String BitFlagSettings
initSettings config =
    let
        flags : List String
        flags =
            config.flags
                |> List.map String.trim
                |> List.filter (\s -> not (String.isEmpty s))
                |> List.map String.toLower

        uniqueFlags : Set String
        uniqueFlags =
            flags
                |> Set.fromList

        flagsWithEmptyBitSpaces : Array String
        flagsWithEmptyBitSpaces =
            config.flags
                |> List.map (\s -> String.trim s)
                |> List.map (\s -> String.toLower s)
                |> Array.fromList

        flagMap : Array (Maybe String)
        flagMap =
            Array.initialize config.bitLimit (\n -> Array.get n flagsWithEmptyBitSpaces)
                |> Array.map transformEmptyToNothing
    in
    if Set.size uniqueFlags /= List.length flags then
        Err "Duplicate flags detected"

    else
        Ok flagMap


showAllFlags : BitFlagSettings -> List String
showAllFlags _ =
    []


createFlag : String -> BitFlagSettings -> Result String BitFlagSettings
createFlag _ settings =
    Ok settings


updateFlag : String -> String -> BitFlagSettings -> BitFlagSettings
updateFlag _ _ settings =
    settings


deleteFlag : String -> BitFlagSettings -> BitFlagSettings
deleteFlag _ settings =
    settings



-- Register Functions


enabledFlags : BitFlagSettings -> Int -> List String
enabledFlags settings register =
    [ "placeholder", "flags", "for now" ]


enableFlag : BitFlagSettings -> String -> Int -> Int
enableFlag settings flag register =
    case findFlagIndex (Array.toIndexedList settings) flag of
        Just index ->
            Bitwise.or register (2 ^ index)

        Nothing ->
            register


flipFlag : BitFlagSettings -> String -> Int -> Int
flipFlag settings flag register =
    case findFlagIndex (Array.toIndexedList settings) flag of
        Just index ->
            Bitwise.xor register (2 ^ index)

        Nothing ->
            register


query : BitFlagSettings -> List String -> List String -> Int -> Bool
query settings whitelist blacklist register =
    let
        flagIndexFinder =
            findFlagIndex (Array.toIndexedList settings)

        registerBuilder chosenList =
            chosenList
                |> Set.fromList
                |> Set.toList
                |> List.map (\flag -> flagIndexFinder flag)
                |> List.foldl
                    (\maybeIndex acc ->
                        case maybeIndex of
                            Just bitIndex ->
                                acc + 2 ^ bitIndex

                            Nothing ->
                                acc
                    )
                    0

        whitelistRegister =
            registerBuilder whitelist

        blacklistRegister =
            registerBuilder blacklist
    in
    (Bitwise.and register whitelistRegister == whitelistRegister)
        && (Bitwise.and register blacklistRegister == 0)



-- Helper functions


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
