module Tests exposing (..)

import Array exposing (Array)
import BitFlags exposing (..)
import Expect
import Test exposing (..)


rawFlags : List String
rawFlags =
    [ "red"
    , "black"
    , "  "
    , "blue"
    , "green"
    , ""
    , "yellow"
    , "purple"
    , "pink"
    , "orange"
    ]


testInitSettings : Test
testInitSettings =
    Test.describe "BitFlags.initSettings"
        [ test "runs with empty bit spaces"
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
                        12
                        rawFlags
                    )
            )
        , test "bitLimit is enforced"
            (\_ ->
                Expect.equal
                    (Ok
                        (Array.fromList
                            [ Just "red"
                            , Just "black"
                            , Nothing
                            ]
                        )
                    )
                    (BitFlags.initSettings
                        3
                        rawFlags
                    )
            )
        , test "errors out on duplicate flags"
            (\_ -> Expect.err (BitFlags.initSettings 4 ("red" :: "red" :: rawFlags)))
        ]


bitLimit : Int
bitLimit =
    32


bitFlagSettings : BitFlagSettings
bitFlagSettings =
    case BitFlags.initSettings bitLimit rawFlags of
        Ok settings ->
            settings

        Err _ ->
            Array.fromList []


testCreateFlag : Test
testCreateFlag =
    Test.describe "BitFlags.createFlag"
        [ test "runs"
            (\_ -> Expect.ok <| createFlag bitFlagSettings "runs")
        , test "adds flag to settings"
            (\_ ->
                Expect.equal
                    (initSettings 1 [ "maroon" ])
                    (initSettings 1 []
                        |> Result.andThen (\setting -> createFlag setting "maroon")
                    )
            )
        , test "should make use of empty bit space"
            (\_ ->
                Expect.equal
                    (initSettings 3 [ "one", "maroon", "three" ])
                    (initSettings 3 [ "one", "", "three" ]
                        |> Result.andThen (\setting -> createFlag setting "maroon")
                    )
            )
        , test "errors out when out of bit spaces"
            (\_ ->
                Expect.err
                    (initSettings 3 [ "one", "two", "three" ]
                        |> Result.andThen (\setting -> createFlag setting "maroon")
                    )
            )
        ]


testDeleteFlag : Test
testDeleteFlag =
    Test.describe "BitFlags.deleteFlag"
        [ test "removes flag from settings"
            (\_ ->
                Expect.equal
                    (initSettings 1 [ "" ])
                    (Result.map
                        (\setting -> deleteFlag setting "maroon")
                        (initSettings 1 [ "maroon" ])
                    )
            )
        , test "should make use of empty bit space"
            (\_ ->
                Expect.equal
                    (initSettings 3 [ "one", "", "three" ])
                    (Result.map
                        (\setting -> deleteFlag setting "maroon")
                        (initSettings 3 [ "one", "maroon", "three" ])
                    )
            )
        , test "idempotent"
            (\_ ->
                Expect.equal
                    (initSettings 3 [ "one", "", "three" ])
                    (Result.map
                        (\setting -> deleteFlag setting "maroon")
                        (initSettings 3 [ "one", "", "three" ])
                    )
            )
        ]


showEnabledFlags : Int -> List String
showEnabledFlags =
    BitFlags.showEnabledFlagsOnRegister bitFlagSettings


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


enableFlagOnRegister : String -> (Int -> Int)
enableFlagOnRegister =
    addToRegister bitFlagSettings


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


disableFlagOnRegister : String -> (Int -> Int)
disableFlagOnRegister =
    removeFromRegister bitFlagSettings


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



-- TODO testRemoveFlagFromBitState
-- TODO start renaming concepts, should be BitFlags.Map and BitFlags.Register
-- TODO filter list of states for a given flag name
-- TODO filter list of states for a list of flag names
-- TODO filter list of states for a negation of a flag name
-- TODO filter list of states for a list of flag names as well as some negations
-- TODO BitFlags.createFlag -> Result BitFlagMap (Err String)
-- TODO BitFlags.updateFlag -> Result BitFlagMap (Err String)
-- TODO BitFlags.deleteFlag -> Result BitFlagMap (Err String)
-- TODO enforce a package level bit limit, cause elm numbers get a little weird after a point
-- TODO figure out how you want to handle duplicates
-- TODO settings must be configured in a way where users MUST set up BitFlags through initSettings
