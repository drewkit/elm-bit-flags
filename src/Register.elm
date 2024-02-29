module Register exposing
    ( disableFlag
    , enableFlag
    , enabledFlags
    , flipFlag
    , query
    )

import Array exposing (Array)
import Bitwise
import Helpers exposing (..)
import Set


type BitFlagSettings
    = BitFlagSettings (Array (Maybe String))


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
