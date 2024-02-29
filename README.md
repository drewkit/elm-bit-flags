# Elm Bit Flags


Once a flag is removed from bit flag settings, you are responsible for ensuring that bit flag in question is removed from all known registries. This ensures that creating a new flag that utilizes this bit space doesn't overlap with previously enabled bits.

```
settings =
    initSettings {bitLimit = 5, flags = ["golf", "tennis", "hiking"]}
        |> Result.withDefault defaultSettings
        |> createFlag "pickleball"
        |> updateFlag "hiking" "backpacking"
        |> deleteFlag "golf"

showAllFlags settings
#=> ["tennis", "backpacking", "pickleball"]

enableFlag =
    addToRegister settings

disableFlag =
    removeFromRegister settings

showEnabledFlags =
    showEnabledFlagsOnRegister settings

emptyRegister = 0

sampleRegister =
    emptyRegister
        |> enableFlag "pickleball" -- #=> 8
        |> enableFlag "tennis" -- #=> 10
    
query settings ["pickleball", "tennis"] [] sampleRegister
-- True
    
query settings ["pickleball"] ["tennis"] sampleRegister
-- False

query settings ["backpacking", "tennis"] [] sampleRegister
-- False
```