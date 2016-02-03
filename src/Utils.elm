module Utils where

import Date
import Date.Format
import Date.Config
import Date.Create
import Date.Config.Config_en_us exposing (..)
import Vendor
import Color exposing (Color, toHsl, hsla, toRgb, rgba)
import String

global_tzone = Date.fromTime 0 |> Date.Create.getTimezoneOffset |> (*) 60000 |> (-) 0 |> toFloat

timeFromString str =  
    let
        t = str |> Date.fromString |> Result.withDefault (Date.fromTime 0) |> Date.toTime
    in 
        if Vendor.prefix == Vendor.Webkit
        then t 
        else t + global_tzone

timeToString t = Date.fromTime (t - global_tzone) |> Date.Format.format Date.Config.Config_en_us.config "%H:%M:%S"

toCssString : Color -> String
toCssString cl =
    let
        { red, green, blue, alpha } = toRgb cl
        rgba =
            [ (toFloat red), (toFloat green), (toFloat blue), alpha ]
                |> List.map toString
                |> (String.join ",")
    in
        "rgba(" ++ rgba ++ ")"