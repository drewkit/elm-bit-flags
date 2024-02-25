module Tests exposing (..)

import Array exposing (Array)
import BitFlags exposing (..)
import Expect
import Test exposing (..)


rawFlags : List String
rawFlags =
    [ "Red" -- 1
    , "black" -- 2
    , "  " -- 4
    , "blue" -- 8
    , "green" -- 16
    , "" -- 32
    , "yellow" -- 64
    , "purple" -- 128
    , "pink" -- 256
    , "orange" -- 512
    ]


testInitSettings : Test
testInitSettings =
    Test.describe "BitFlags.initSettings"
        [ test "runs with empty bit spaces, eliminating case sensitivities"
            (\_ ->
                Expect.equal
                    (Ok
                        (Array.fromList
                            [ Just "red"
                            , Just "black"
                            , Nothing
                            , Just "blue"
                            , Just "green"
                            , Nothing
                            , Just "yellow"
                            , Just "purple"
                            , Just "pink"
                            , Just "orange"
                            , Nothing
                            , Nothing
                            ]
                        )
                    )
                    (BitFlags.initSettings
                        { bitLimit = 12
                        , flags = rawFlags
                        }
                    )
            )
        , test "bitLimit is enforced"
            (\_ ->
                Expect.err (BitFlags.initSettings { bitLimit = 3, flags = rawFlags })
            )
        , test "errors out on duplicate flags"
            (\_ -> Expect.err (BitFlags.initSettings { bitLimit = 4, flags = "Red" :: "red" :: rawFlags }))
        ]


bitLimit : Int
bitLimit =
    32


bitFlagSettings : BitFlagSettings
bitFlagSettings =
    case BitFlags.initSettings { bitLimit = bitLimit, flags = rawFlags } of
        Ok settings ->
            settings

        Err _ ->
            Array.fromList []


testCreateFlag : Test
testCreateFlag =
    Test.describe "BitFlags.createFlag"
        [ test "runs"
            (\_ -> Expect.ok <| createFlag "a flag" bitFlagSettings)
        , test "eliminates case sensitivities"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 1, flags = [ "maroon" ] })
                    (initSettings { bitLimit = 1, flags = [] }
                        |> Result.andThen (\setting -> createFlag "mAroon" setting)
                    )
            )
        , test "should make use of empty bit space"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 3, flags = [ "one", "maroon", "three" ] })
                    (initSettings { bitLimit = 3, flags = [ "one", "", "three" ] }
                        |> Result.andThen (\setting -> createFlag "maroon" setting)
                    )
            )
        , test "takes the lowest indexed position"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 4, flags = [ "one", "maroon", "three", "" ] })
                    (initSettings { bitLimit = 4, flags = [ "one", "", "three", "" ] }
                        |> Result.andThen (\setting -> createFlag "maroon" setting)
                    )
            )
        , test "errors out on empty flag"
            (\_ ->
                Expect.err
                    (initSettings { bitLimit = 4, flags = [ "one", "", "three", "" ] }
                        |> Result.andThen (\setting -> createFlag " " setting)
                    )
            )
        , test "errors out when out of bit spaces"
            (\_ ->
                Expect.err
                    (initSettings { bitLimit = 3, flags = [ "one", "two", "three" ] }
                        |> Result.andThen (\setting -> createFlag "maroon" setting)
                    )
            )
        ]


testUpdateFlag : Test
testUpdateFlag =
    Test.describe "BitFlags.updateFlag"
        [ test "updates a given flag name"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 4, flags = [ "", "one fish", "two fish", "" ] })
                    (initSettings { bitLimit = 4, flags = [ "", "red fish", "blue fish", "" ] }
                        |> Result.andThen (\setting -> Ok (updateFlag "red fish" "one fish" setting))
                        |> Result.andThen (\setting -> Ok (updateFlag "blue fish" "two fish" setting))
                    )
            )
        , test "ignores unrecognized flags"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 4, flags = [ "", "one fish", "two fish", "" ] })
                    (initSettings { bitLimit = 4, flags = [ "", "one fish", "two fish", "" ] }
                        |> Result.andThen (\setting -> Ok (updateFlag "red fish" "blue fish" setting))
                    )
            )
        , test "also ignores blank strings"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 4, flags = [ "", "one fish", "two fish", "" ] })
                    (initSettings { bitLimit = 4, flags = [ "", "one fish", "two fish", "" ] }
                        |> Result.andThen (\setting -> Ok (updateFlag "" "more fish" setting))
                    )
            )
        , test "eliminates case sensitivy in query"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 4, flags = [ "dog", "one fish", "two fish", "" ] })
                    (initSettings { bitLimit = 4, flags = [ "mouse", "one fish", "two fish", "" ] }
                        |> Result.andThen (\setting -> Ok (updateFlag "dOG" "mouse" setting))
                    )
            )
        , test "eliminates case sensitivy in updated value"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 4, flags = [ "dog", "one fish", "two fish", "" ] })
                    (initSettings { bitLimit = 4, flags = [ "mouse", "one fish", "two fish", "" ] }
                        |> Result.andThen (\setting -> Ok (updateFlag "dog" "mouSE" setting))
                    )
            )
        , test "ignores blank strings with spaces"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 4, flags = [ " ", "one fish", "two fish", "" ] })
                    (initSettings { bitLimit = 4, flags = [ " ", "one fish", "two fish", "" ] }
                        |> Result.andThen (\setting -> Ok (updateFlag " " "more fish" setting))
                    )
            )
        ]


testDeleteFlag : Test
testDeleteFlag =
    Test.describe "BitFlags.deleteFlag"
        [ test "removes flag from settings"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 1, flags = [ "" ] })
                    (Result.map
                        (\setting -> deleteFlag "maroon" setting)
                        (initSettings { bitLimit = 1, flags = [ "maroon" ] })
                    )
            )
        , test "should make use of empty bit space"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 3, flags = [ "one", "", "three" ] })
                    (Result.map
                        (\setting -> deleteFlag "maroon" setting)
                        (initSettings { bitLimit = 3, flags = [ "one", "maroon", "three" ] })
                    )
            )
        , test "case insensitive query"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 3, flags = [ "one", "", "three" ] })
                    (Result.map
                        (\setting -> deleteFlag "maRoon" setting)
                        (initSettings { bitLimit = 3, flags = [ "one", "maroon", "three" ] })
                    )
            )
        , test "idempotent"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 3, flags = [ "one", "", "three" ] })
                    (Result.map
                        (\setting -> deleteFlag "maroon" setting)
                        (initSettings { bitLimit = 3, flags = [ "one", "", "three" ] })
                    )
            )
        ]


showEnabledFlags : Int -> List String
showEnabledFlags =
    BitFlags.enabledFlags bitFlagSettings


testShowEnabledFlagsOnRegister : Test
testShowEnabledFlagsOnRegister =
    Test.describe "BitFlags.showEnabledFlagsOnRegister"
        [ test "runs"
            (\_ -> Expect.equal [ "purple" ] (showEnabledFlags 128))
        , test "handles multiple bit slots"
            (\_ -> Expect.equal [ "red", "purple" ] (showEnabledFlags 130))
        , test "ignores empty bit slots"
            (\_ -> Expect.equal [ "red", "purple" ] (showEnabledFlags 134))
        , test "will only operate with bits that are within scope of the bit limit"
            (\_ -> Expect.equal [ "red" ] (showEnabledFlags ((2 ^ (bitLimit + 1)) + 2)))
        ]


enableFlagOnRegister =
    enableFlag bitFlagSettings


testEnableFlagOnRegister : Test
testEnableFlagOnRegister =
    Test.describe "BitFlags.addToRegister"
        [ test "run1"
            (\_ -> Expect.equal 8 (enableFlagOnRegister "blue" 0))
        , test "run2"
            (\_ -> Expect.equal 10 (enableFlagOnRegister "blue" 2))
        , test "no changes to register when flag is not present on map"
            (\_ -> Expect.equal 2 (enableFlagOnRegister "poppycock" 2))
        , test "enabled bits on register for nonexistent flags will also be corrected"
            (\_ -> Expect.equal 0 (enableFlagOnRegister "poppycock" 4))
        ]


disableFlagOnRegister =
    disableFlag bitFlagSettings


testDisableFlagOnRegister : Test
testDisableFlagOnRegister =
    Test.describe "BitFlags.removeFromRegister"
        [ test "run1"
            (\_ -> Expect.equal 0 (disableFlagOnRegister "blue" 8))
        , test "run2"
            (\_ -> Expect.equal 2 (disableFlagOnRegister "blue" 10))
        , test "no changes to register when flag is not present on map"
            (\_ -> Expect.equal 2 (disableFlagOnRegister "poppycock" 2))
        , test "enabled bits on register for nonexistent flags will also be corrected"
            (\_ -> Expect.equal 0 (disableFlagOnRegister "poppycock" 4))
        ]


testShowAllFlags : Test
testShowAllFlags =
    Test.describe "BitFlags.showAllFlags"
        [ test "run1"
            (\_ ->
                Expect.equal (showAllFlags bitFlagSettings)
                    [ "red"
                    , "black"
                    , "blue"
                    , "green"
                    , "yellow"
                    , "purple"
                    , "pink"
                    , "orange"
                    ]
            )
        ]


isFlagMatch : Int -> Bool
isFlagMatch =
    query bitFlagSettings [ "red", "red", "black", "magenta" ] [ "blue" ]


testBuiltRegisterQuery : Test
testBuiltRegisterQuery =
    Test.describe "BitFlags.buildRegisterQuery"
        [ test "Provides Bool on bitmask match, ignoring duplicates (red) and unrecognized flags (magenta)"
            (\_ ->
                Expect.equal True (isFlagMatch 3)
            )
        , test "matches red and black, but also matches blue on the blacklist"
            (\_ ->
                Expect.equal False (isFlagMatch 11)
            )
        , test "matches black, but doesn't match red"
            (\_ ->
                Expect.equal False (isFlagMatch 130)
            )
        , test "matches black, but also matches blue on the blacklist"
            (\_ ->
                Expect.equal False (isFlagMatch 10)
            )
        , test "matches red and black, albeit with an unregistered flag included"
            (\_ ->
                Expect.equal False (isFlagMatch 7)
            )
        , test "matches red and black"
            (\_ ->
                Expect.equal True (isFlagMatch 67)
            )
        , test "fails simply because it doesn't include red (the 1 bit space)"
            (\_ ->
                Expect.equal False (isFlagMatch 58220)
            )
        ]



{--
- TODO enforce a package level bit limit, cause elm numbers get a little weird after a point
- TODO settings must be configured in a way where users MUST set up BitFlagSettings through initSettings
--}
