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


initSettings : Int -> List String -> Result String BitFlagSettings
initSettings bitLimit rawFlags =
    let
        flagsWithoutEmptyBitSpaces : List String
        flagsWithoutEmptyBitSpaces =
            List.map String.trim rawFlags
                |> List.filter (\s -> not (String.isEmpty s))

        uniqueFlags : Set String
        uniqueFlags =
            flagsWithoutEmptyBitSpaces
                |> Set.fromList

        flagsWithEmptyBitSpaces : Array String
        flagsWithEmptyBitSpaces =
            rawFlags
                |> Array.fromList

        flagMap : Array (Maybe String)
        flagMap =
            Array.initialize bitLimit (\n -> Array.get n flagsWithEmptyBitSpaces)
                |> Array.map transformEmptyToNothing
    in
    if Set.size uniqueFlags /= List.length flagsWithoutEmptyBitSpaces then
        Err "Duplicate flags detected"

    else
        Ok flagMap


createFlag : BitFlagSettings -> String -> Result String BitFlagSettings
createFlag settings flag =
    Ok settings


deleteFlag : BitFlagSettings -> String -> BitFlagSettings
deleteFlag settings flag =
    settings


showEnabledFlagsOnRegister : BitFlagSettings -> Int -> List String
showEnabledFlagsOnRegister settings register =
    [ "placeholder", "flags", "for now" ]


addToRegister : BitFlagSettings -> String -> Int -> Int
addToRegister _ _ _ =
    42


removeFromRegister : BitFlagSettings -> String -> Int -> Int
removeFromRegister _ _ _ =
    42
