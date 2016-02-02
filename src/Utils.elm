module Utils where

import Date
import Date.Format
import Date.Config
import Date.Create
import Date.Config.Config_en_us exposing (..)

global_tzone = Date.fromTime 0 |> Date.Create.getTimezoneOffset |> (*) 60000 |> (-) 0 |> toFloat

timeFromString str =  str |> Date.fromString |> Result.withDefault (Date.fromTime 0) |> Date.toTime |> (+) global_tzone 
timeToString t = Date.fromTime (t - global_tzone) |> Date.Format.format Date.Config.Config_en_us.config "%H:%M:%S"