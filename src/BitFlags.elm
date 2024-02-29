module BitFlags exposing
    ( BitFlagSettings
    , allFlags
    , createFlag
    , defaultSettings
    , deleteFlag
    , disableFlag
    , enableFlag
    , enabledFlags
    , flipFlag
    , initSettings
    , query
    , updateFlag
    )

import Array exposing (Array)
import Bitwise
import Helpers exposing (..)
import Register exposing (..)
import Set exposing (Set)


type BitFlagSettings
    = BitFlagSettings (Array (Maybe String))


duplicateFlagsFound : Array String -> Bool
duplicateFlagsFound rawFlags =
    let
        flags : Array String
        flags =
            rawFlags
                |> Array.map sanitizeFlag
                |> Array.filter (\s -> not (String.isEmpty s))

        uniqueFlags : Set String
        uniqueFlags =
            flags
                |> Array.toList
                |> Set.fromList
    in
    Set.size uniqueFlags /= Array.length flags


allFlags : BitFlagSettings -> List String
allFlags (BitFlagSettings settings) =
    settings
        |> Array.toList
        |> List.foldr
            (\maybeVal acc ->
                case maybeVal of
                    Just val ->
                        val :: acc

                    Nothing ->
                        acc
            )
            []


defaultSettings : BitFlagSettings
defaultSettings =
    BitFlagSettings <| Array.fromList []


initSettings : { bitLimit : Int, flags : List String } -> Result String BitFlagSettings
initSettings config =
    let
        flagsWithEmptyBitSpaces : Array String
        flagsWithEmptyBitSpaces =
            config.flags
                |> List.map sanitizeFlag
                |> Array.fromList
    in
    if duplicateFlagsFound flagsWithEmptyBitSpaces then
        Err "Duplicate flags detected"

    else if config.bitLimit > 32 then
        Err "bitLimit cannot exceed 32 bits"

    else if Array.length flagsWithEmptyBitSpaces > config.bitLimit then
        Err "Flags list exceeds bit space limit"

    else
        let
            fullBitSpaceFlagArray =
                Array.initialize
                    config.bitLimit
                    (\n -> Array.get n flagsWithEmptyBitSpaces)
        in
        Ok <|
            BitFlagSettings <|
                (fullBitSpaceFlagArray |> Array.map transformEmptyFlagStringToNothingVal)


createFlag : String -> BitFlagSettings -> Result String BitFlagSettings
createFlag rawFlag (BitFlagSettings settings) =
    let
        flag =
            sanitizeFlag rawFlag

        preExistingFlagIndex =
            findFlagIndex (Array.toIndexedList settings) flag
    in
    case preExistingFlagIndex of
        Just _ ->
            Err "Flag already exists"

        Nothing ->
            if flag == "" then
                Err "Flag cannot be blank"

            else
                case findFirstNothing (Array.toIndexedList settings) of
                    Nothing ->
                        Err "Out of bit empty bit spaces"

                    Just index ->
                        Ok <|
                            BitFlagSettings <|
                                Array.set index (Just flag) settings


updateFlag : String -> String -> BitFlagSettings -> BitFlagSettings
updateFlag originalRawFlag updatedRawFlag (BitFlagSettings settings) =
    let
        originalFlag =
            sanitizeFlag originalRawFlag

        updatedFlag =
            sanitizeFlag updatedRawFlag
    in
    case findFlagIndex (Array.toIndexedList settings) originalFlag of
        Just index ->
            BitFlagSettings <| Array.set index (Just updatedFlag) settings

        Nothing ->
            BitFlagSettings <| settings


deleteFlag : String -> BitFlagSettings -> BitFlagSettings
deleteFlag rawFlag (BitFlagSettings settings) =
    let
        flag =
            sanitizeFlag rawFlag
    in
    case findFlagIndex (Array.toIndexedList settings) flag of
        Just index ->
            BitFlagSettings <| Array.set index Nothing settings

        Nothing ->
            BitFlagSettings <| settings



-- Register Functions


enabledFlags : BitFlagSettings -> Int -> List String
enabledFlags (BitFlagSettings settings) register =
    List.foldr
        (\( flagIndex, maybeVal ) acc ->
            if flagEnabled (2 ^ flagIndex) register then
                case maybeVal of
                    Just val ->
                        val :: acc

                    Nothing ->
                        acc

            else
                acc
        )
        []
        (Array.toIndexedList settings)


enableFlag : BitFlagSettings -> String -> Int -> Int
enableFlag (BitFlagSettings settings) rawFlag register =
    case findFlagIndex (Array.toIndexedList settings) (sanitizeFlag rawFlag) of
        Just index ->
            Bitwise.or register (2 ^ index)

        Nothing ->
            register


disableFlag : BitFlagSettings -> String -> Int -> Int
disableFlag (BitFlagSettings settings) rawFlag register =
    case findFlagIndex (Array.toIndexedList settings) (sanitizeFlag rawFlag) of
        Just index ->
            Bitwise.and (Bitwise.complement (2 ^ index)) register

        Nothing ->
            register


flipFlag : BitFlagSettings -> String -> Int -> Int
flipFlag (BitFlagSettings settings) rawFlag register =
    case findFlagIndex (Array.toIndexedList settings) (sanitizeFlag rawFlag) of
        Just index ->
            Bitwise.xor register (2 ^ index)

        Nothing ->
            register


query : BitFlagSettings -> List String -> List String -> Int -> Bool
query (BitFlagSettings settings) whitelist blacklist register =
    let
        flagIndexFinder =
            findFlagIndex (Array.toIndexedList settings)

        registerBuilder chosenList =
            chosenList
                |> List.map sanitizeFlag
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
