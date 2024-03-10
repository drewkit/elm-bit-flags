module Tests exposing (..)

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
                Expect.ok
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
        , test "bitLimit cannot exceed 32 bits"
            (\_ ->
                Expect.err (BitFlags.initSettings { bitLimit = 33, flags = rawFlags })
            )
        , test "errors out on duplicate flags"
            (\_ -> Expect.err (BitFlags.initSettings { bitLimit = 18, flags = "Red" :: "red" :: rawFlags }))
        ]


bitFlagSettings : BitFlagSettings
bitFlagSettings =
    let
        bitLimit =
            32

        settingsResult =
            BitFlags.initSettings
                { bitLimit = bitLimit
                , flags = rawFlags
                }
    in
    Result.withDefault (defaultSettings 20) settingsResult


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
        , test "errors out on pre-existing flag"
            (\_ ->
                Expect.err
                    (initSettings { bitLimit = 4, flags = [ "one", "", "three", "" ] }
                        |> Result.andThen (\setting -> createFlag "one" setting)
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
                    (initSettings { bitLimit = 4, flags = [ "mouse", "one fish", "two fish", "" ] })
                    (initSettings { bitLimit = 4, flags = [ "dog", "one fish", "two fish", "" ] }
                        |> Result.andThen (\setting -> Ok (updateFlag "dOG" "mouse" setting))
                    )
            )
        , test "eliminates case sensitivy in updated value"
            (\_ ->
                Expect.equal
                    (initSettings { bitLimit = 4, flags = [ "mouse", "one fish", "two fish", "" ] })
                    (initSettings { bitLimit = 4, flags = [ "dog", "one fish", "two fish", "" ] }
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


testShowEnabledFlagsOnRegister : Test
testShowEnabledFlagsOnRegister =
    let
        showEnabledFlags =
            enabledFlags bitFlagSettings
    in
    Test.describe "BitFlags.showEnabledFlagsOnRegister"
        [ test "runs"
            (\_ -> Expect.equal [ "purple" ] (showEnabledFlags 128))
        , test "handles multiple bit slots"
            (\_ -> Expect.equal [ "red", "purple" ] (showEnabledFlags 129))
        , test "ignores empty bit slots"
            (\_ -> Expect.equal [ "red", "purple" ] (showEnabledFlags 133))
        , test "handles more bit slots"
            (\_ -> Expect.equal [ "red", "blue", "purple" ] (showEnabledFlags 137))
        ]


testEnableFlagOnRegister : Test
testEnableFlagOnRegister =
    let
        enableFlagOnRegister =
            enableFlag bitFlagSettings
    in
    Test.describe "BitFlags.enableFlag"
        [ test "run1"
            (\_ -> Expect.equal 8 (enableFlagOnRegister "Blue" 0))
        , test "run2"
            (\_ -> Expect.equal 10 (enableFlagOnRegister "blue " 2))
        , test "no changes to register when flag is not present on map"
            (\_ -> Expect.equal 2 (enableFlagOnRegister "poppycock" 2))
        , test "is idempotent"
            (\_ -> Expect.equal 8 (enableFlagOnRegister "bluE" 8))
        ]


testDisableFlagOnRegister : Test
testDisableFlagOnRegister =
    let
        disableFlagOnRegister =
            disableFlag bitFlagSettings
    in
    Test.describe "BitFlags.disableFlag"
        [ test "run1"
            (\_ -> Expect.equal 0 (disableFlagOnRegister "bluE " 8))
        , test "run2"
            (\_ -> Expect.equal 2 (disableFlagOnRegister "blue" 10))
        , test "no changes to register when flag is not present on map"
            (\_ -> Expect.equal 2 (disableFlagOnRegister "poppycock" 2))
        , test "is idempotent"
            (\_ -> Expect.equal 0 (disableFlagOnRegister "blue" 0))
        ]


testFlipFlagOnRegister : Test
testFlipFlagOnRegister =
    let
        flipFlagOnRegister =
            flipFlag bitFlagSettings
    in
    Test.describe "BitFlags.flipFlag"
        [ test "run1"
            (\_ -> Expect.equal 0 (flipFlagOnRegister "bluE " 8))
        , test "run2"
            (\_ -> Expect.equal 2 (flipFlagOnRegister "blue" 10))
        , test "no changes to register when flag is not present on map"
            (\_ -> Expect.equal 2 (flipFlagOnRegister "poppycock" 2))
        , test "run4"
            (\_ -> Expect.equal 8 (flipFlagOnRegister "blue" 0))
        ]


testShowAllFlags : Test
testShowAllFlags =
    Test.describe "BitFlags.allFlags"
        [ test "run1"
            (\_ ->
                Expect.equal (allFlags bitFlagSettings)
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
    match bitFlagSettings [ "red", "Red", " blacK", "magenta" ] [ "blue" ]


testBuiltRegisterQuery : Test
testBuiltRegisterQuery =
    Test.describe "BitFlags.query"
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
                Expect.equal True (isFlagMatch 7)
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


testSerializeFlagSpace : Test
testSerializeFlagSpace =
    Test.describe "BitFlags.serialize"
        [ test "Provides a list of current bit flag space that can be inputted to a subsequent initSettings"
            (\_ ->
                let
                    inputList =
                        [ "", "red fish", "", "blue fish", "" ]
                in
                Expect.equal
                    inputList
                    (initSettings { bitLimit = 5, flags = inputList }
                        |> Result.andThen (\setting -> Ok (BitFlags.serialize setting))
                        |> Result.withDefault []
                    )
            )
        ]
