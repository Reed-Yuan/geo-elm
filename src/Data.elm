module Data where

import List
import Signal
import String
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (..)
import Maybe exposing (..)
import Utils
import TileMap

type alias VehiclTrace = (Int, String, Color, Form, Form, List TileMap.Gpsx)

parseGps : List (Int, String, Float, Float, Float, Float) -> Color 
            -> (Color -> Int -> Html) -> TileMap.Map -> VehiclTrace
parseGps gpsxRaw colr icn mapp =
    let 
        parseRow (vid, timeStr, lat, lon, speed, direction) = 
            {vehicleId = vid
            , vehicleName = vid - 2000000 |> toString |> String.append "#"
            , timestamp = Utils.timeFromString timeStr
            , lat = lat, lon = lon, speed = speed, direction = direction}
        isValidTime x = x.timestamp > 0
        isSameId g1 g2 = g1.vehicleId == g2.vehicleId
        emptyForm = toForm empty
        process x = if List.isEmpty x then (-1, "", Color.red, Graphics.Element.empty  |> toForm, emptyForm, []) else
                        case List.head x of
                            Just gps -> 
                                let
                                    sortedGps = List.sortBy .timestamp x
                                    fullTrace = TileMap.path sortedGps mapp {defaultLine | color = colr, width = 10}
                                in
                                    (gps.vehicleId, gps.vehicleName, colr, (icn colr 24) |> Html.toElement 24 24 |> toForm, fullTrace, sortedGps) 
                            _ -> (-1, "", Color.red, Graphics.Element.empty  |> toForm, emptyForm, [])
        gpsx = gpsxRaw |> List.map parseRow |> List.filter isValidTime |> process
    in
        gpsx