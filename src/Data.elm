module Data where

import List
import Signal
import String
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (..)
import Utils exposing (..)
import TileMap
import Text
import Time exposing (..)

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
        

showTrace: VehiclTrace -> Time -> Int -> TileMap.Map -> (Form, Element)
showTrace (_, vname, colr, icn, _, gps) t tcLength mapp = 
    let
        trace = List.filter (\g -> g.timestamp < t && t - g.timestamp <= (if tcLength == 0 then 24 else tcLength) * 60000) gps |> List.reverse
        emptyForm = Graphics.Element.empty  |> toForm
        head = case (List.head trace) of
            Just g -> 
                let
                    (x, y) = TileMap.proj (g.lat, g.lon) mapp
                    p = move (x, y) icn
                    n = vname |> Text.fromString 
                        |> outlinedText {defaultLine | width = 1, color = colr} 
                        |> move (x + 40, y + 20)
                in 
                    (Graphics.Collage.group [p, n], showInfo g (toCssString colr))
            _ -> (emptyForm, Graphics.Element.empty)
        hstE = 
            if tcLength == 0 then emptyForm
            else TileMap.path trace mapp {defaultLine | color = colr, width = 6, dashing=[8, 4]}
    in 
        (Graphics.Collage.group [fst head, hstE], snd head)

showInfo g colorr = 
  (div [style [("padding-left", "20px"), ("color", colorr), ("background-color", "rgba(255, 255, 255, 0.85)")]] <|
        Html.span [style [("font-size", "large"), ("font-weight", "bold"), ("color", toString colorr)]] [Html.text g.vehicleName]
        :: br [] []
        :: br [] []
        :: Html.span [style [("font-size", "large"), ("color", colorr)]] [Html.text ("Time: " ++ timeToString g.timestamp)]
        :: br [] []
        :: Html.span [style [("font-size", "large"), ("color", colorr)]] [Html.text ("Lat: " ++ toString g.lat)]
        :: br [] []
        :: Html.span [style [("font-size", "large"), ("color", colorr)]] [Html.text ("Lon: " ++ toString g.lon)]
        :: br [] []
        :: Html.span [style [("font-size", "large"), ("color", colorr)]] [Html.text ("Speed: " ++ toString g.speed)]
        :: br [] []
        :: [Html.span [style [("font-size", "large"), ("color", colorr)]] [Html.text ("Direction: " ++ toString g.direction)]]
        )
    |> (Html.toElement 160 160)
        