# Elm Bit Flags


```
import BitFlags as BF exposing (BitFlagSettings)

settings =
    BF.initSettings {bitLimit = 5, flags = ["golf", "tennis", "hiking"]}
        |> Result.withDefault BF.defaultSettings
        |> BF.createFlag "pickleball"
        |> Result.withDefault BF.defaultSettings
        |> BF.updateFlag "hiking" "backpacking"
        |> BF.deleteFlag "golf"

BF.allFlags settings
#=> ["tennis", "backpacking", "pickleball"]

enableFlag =
    BF.enableFlag settings

disableFlag =
    BF.disableFlag settings

showEnabledFlags =
    BF.enabledFlags settings

emptyRegister = 0

sampleRegister =
    emptyRegister
        |> enableFlag "pickleball" -- #=> 8
        |> enableFlag "tennis" -- #=> 10
    
match =
   BF.match settings
 
match ["pickleball", "tennis"] [] sampleRegister
-- True
    
match ["pickleball"] ["tennis"] sampleRegister
-- False

match ["backpacking", "tennis"] [] sampleRegister
-- False
```