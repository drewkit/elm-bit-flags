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
    
query =
   BF.query settings
 
query ["pickleball", "tennis"] [] sampleRegister
-- True
    
query ["pickleball"] ["tennis"] sampleRegister
-- False

query ["backpacking", "tennis"] [] sampleRegister
-- False
```