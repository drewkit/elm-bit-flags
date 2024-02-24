module Tests exposing (..)

import Array exposing (Array)
import BitFlags exposing (..)
import Expect
import Test exposing (..)


rawFlags : List String
rawFlags =
    [ "red"
    , "red"
    , "  "
    , "blue"
    , "green"
    , ""
    , "yellow"
    , "purple"
    , "pink"
    , "orange"
    ]


testInit : Test
testInit =
    Test.describe "BitFlags.init"
        [ test "runs with empty bit spaces"
            (\_ ->
                Expect.equal
                    (Array.fromList
                        [ Just "red"
                        , Just "red"
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
                    (BitFlags.init
                        12
                        rawFlags
                    )
            )
        , test "bitLimit is enforced"
            (\_ ->
                Expect.equal
                    (Array.fromList
                        [ Just "red"
                        , Just "red"
                        , Nothing
                        ]
                    )
                    (BitFlags.init
                        3
                        rawFlags
                    )
            )
        ]


bitLimit : Int
bitLimit =
    32


bitFlagMap : BitFlagMap
bitFlagMap =
    BitFlags.init bitLimit rawFlags


showFlags : Int -> List String
showFlags =
    BitFlags.showEnabledFlags bitFlagMap


testShowEnabledFlags : Test
testShowEnabledFlags =
    Test.describe "returns list of flags based on registered state"
        [ test "runs"
            (\_ -> Expect.equal [ "purple" ] (showFlags 128))
        , test "handles multiple bit slots"
            (\_ -> Expect.equal [ "red", "purple" ] (showFlags 130))
        , test "ignores empty bit slots"
            (\_ -> Expect.equal [ "red", "purple" ] (showFlags 134))
        , test "will only operate with bits that are within scope of the bit limit"
            (\_ -> Expect.equal [ "red" ] (showFlags ((2 ^ (bitLimit + 1)) + 2)))
        ]


enableFlagOnRegister : String -> (Int -> Int)
enableFlagOnRegister flag =
    addToRegister bitFlagMap flag


testaddFlagToRegister : Test
testaddFlagToRegister =
    Test.describe "provide an updated integer value when adding or removing a flag"
        [ test "enabling the bit of a known flag to a register"
            (\_ -> Expect.equal 8 (enableFlagOnRegister "blue" 0))
        , test "run2"
            (\_ -> Expect.equal 10 (enableFlagOnRegister "blue" 2))
        , test "no changes to register when flag is not present on map"
            (\_ -> Expect.equal 2 (enableFlagOnRegister "poppycock" 2))
        , test "enabled bits on register for nonexistent flags will also be corrected"
            (\_ -> Expect.equal 0 (enableFlagOnRegister "poppycock" 4))
        ]



-- TODO change it up, only initialize bitFlagMaps with Sets
-- NOPE, nevermind, every flag deletion from the map would create CHAOS
-- TODO testRemoveFlagFromBitState
-- TODO start renaming concepts, should be BitFlags.Map and BitFlags.Register
-- TODO filter list of states for a given flag name
-- TODO filter list of states for a list of flag names
-- TODO filter list of states for a negation of a flag name
-- TODO filter list of states for a list of flag names as well as some negations
-- TODO BitFlags.createFlag -> Result BitFlagMap (Err String)
-- TODO BitFlags.updateFlag -> Result BitFlagMap (Err String)
-- TODO BitFlags.deleteFlag -> Result BitFlagMap (Err String)
