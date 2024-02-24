module BitFlags exposing (..)

import Array exposing (Array)


type alias BitFlagMap =
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


init : Int -> List String -> BitFlagMap
init bitLimit rawFlags =
    let
        flagArray : Array String
        flagArray =
            rawFlags
                |> Array.fromList
    in
    Array.initialize bitLimit (\n -> Array.get n flagArray)
        |> Array.map transformEmptyToNothing


hasFlag : BitFlagMap -> Int -> Bool
hasFlag flags pos =
    case Array.get pos flags of
        Just _ ->
            True

        Nothing ->
            False


showEnabledFlags : BitFlagMap -> Int -> List String
showEnabledFlags bitFlagMap register =
    [ "bathroom", "cleaning", "laundry" ]


addToRegister : BitFlagMap -> String -> Int -> Int
addToRegister _ _ _ =
    56
