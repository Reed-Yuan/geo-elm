module Utils where

import Date
import Date.Create

global_tzone = Date.fromTime 0 |> Date.Create.getTimezoneOffset |> (*) 60000 |> (-) 0 |> toFloat

timeFromString str =  str |> Date.fromString |> Result.withDefault (Date.fromTime 0) |> Date.toTime |> (+) global_tzone 