module BitFlags exposing (..)

import Array exposing (Array)
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
            List.map String.trim config.flags
                |> List.filter (\s -> not (String.isEmpty s))
                |> List.map (\s -> String.toLower s)

        uniqueFlags : Set String
        uniqueFlags =
            flags
                |> Set.fromList

        flagsWithEmptyBitSpaces : Array String
        flagsWithEmptyBitSpaces =
            config.flags
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
enableFlag _ _ _ =
    42


disableFlag : BitFlagSettings -> String -> Int -> Int
disableFlag _ _ _ =
    42


query : BitFlagSettings -> List String -> List String -> Int -> Bool
query settings whiteList blackList register =
    -- Bitwise AND operator for both whitelist and blacklist
    -- if whitelist is bit spaces 1 and 2, then REGISTER & 3 == 3 for a match
    -- if blacklist is bit spaces 3 and 4, then REGISTER & 12 == 0 for a match
    -- to summarize for this scenario => (REGISTER & 3 == 3) AND (REGISTER & 12 == 0) returns True
    False
