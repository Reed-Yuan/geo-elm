module Main where 

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import TileMap
import FontAwesome
import Html
import Html.Attributes exposing (..)
import Time exposing (..)
import Set exposing (..)
import Text
import Graphics.Input
import Signal.Extra exposing (..)
import Drag exposing (..)

import Data exposing (..)
import MapControl exposing (..)
import VideoControl exposing (..)
import VehicleControl exposing (..)
import Widget

port vehicleIn : Signal (List (List (Int, String, Float, Float, Float, Float)))

port mouseWheelIn : Signal MouseWheel
port screenSizeIn : Signal (Int, Int)

global_colors : List Color
global_colors = [Color.red, Color.blue, Color.brown, Color.orange, Color.darkGreen]
global_icons : List (Color -> Int -> Html.Html)
global_icons = [FontAwesome.truck, FontAwesome.ambulance, FontAwesome.taxi, FontAwesome.motorcycle, FontAwesome.bus]

shadowFlow : Signal.Mailbox Bool
shadowFlow = Signal.mailbox False

mapMoveEvt =
    let
        mergedShadow = Signal.mergeMany [VehicleControl.shadowSg, VideoControl.shadowSg]
        check (s, m) = 
            if s then False
            else
                case m of
                    MoveFromTo _ _ -> True
                    _ -> False
                    
        filteredMouseEvt = Signal.Extra.zip mergedShadow Drag.mouseEvents |> Signal.filter check (False, StartAt (0,0)) |> Signal.map snd
    in 
        filteredMouseEvt

mapNetSg = mapSg mouseWheelIn screenSizeIn mapMoveEvt

dataSg : Signal (List VehiclTrace)
dataSg = Signal.map4 (\gps mapp startTime timeDelta -> List.map3 (\x y z -> Data.parseGps x y z mapp startTime timeDelta) gps global_colors global_icons) 
            vehicleIn mapNetSg VideoControl.startTimeSg VideoControl.timeDeltaSg

render : TileMap.Map -> VideoOptions -> List Data.VehiclTrace -> VehicleOptions -> Element
render  mapp videoOptions data vehicleOptions = 
    let
        w = mapp.size |> fst
        h = mapp.size |> snd
        
        (traceAlpha, talpha) = vehicleOptions.traceAlpha
        (mapAlpha, malpha) = vehicleOptions.mapAlpha
        (tailLength, tl) = vehicleOptions.tailLength
        vehicleList = vehicleOptions.selectedVehicles

        baseMap = TileMap.loadMap mapp
        filteredTraces = List.filter (\(id_, _, _, _, _, _) -> Set.member id_ vehicleList) data
        anologClock_ = videoOptions.anologClock |> move ((toFloat w)/2 - 280, (toFloat h)/2 - 70)
        digitClock_ = videoOptions.digitClock |> toForm |> move ((toFloat w)/2 - 100, (toFloat h)/2 - 30)
        progressBar_ = videoOptions.progressBar |> toForm |> move (0, 40 - (toFloat h)/2)
        traceWithInfo = List.map (\vtrace -> showTrace vtrace videoOptions.time tl mapp) filteredTraces |> List.unzip
        vehicleTrace = fst traceWithInfo |> group
        info = (snd traceWithInfo) |> (List.foldr above Graphics.Element.empty) |> container 160 800 (midTopAt (absolute 80) (absolute 0))
                |> toForm |> move ((toFloat w)/2 - 100, 0)
        fullTrace = List.map (\(_, _, _, _, vtrace, _) -> vtrace) filteredTraces |> group |> alpha talpha
        bck = spacer 160 580 |> color white |> opacity 0.85
        checkBoxes_ = checkBoxes vehicleList
        vehicleStateView = layers [bck, 
                            mapAlpha `below` (spacer 1 20) 
                            `below` tailLength `below` (spacer 1 20) 
                            `below` traceAlpha `below` (spacer 1 20)
                            `below` (fst videoOptions.timeDeltaCtl) `below` (spacer 1 20)
                            `below` (fst videoOptions.startTimeCtl) `below` (spacer 1 30)
                            `below` checkBoxes_]
        vehicleStateView_ = vehicleStateView |> toForm |> move (100 - (toFloat w)/2, (toFloat h)/2 - 380)
        gitLink =
                let
                    a = Text.fromString "Source code @GitHub" |> Text.link "https://github.com/Reed-Yuan/geo-elm.git" |> Text.height 22 |> leftAligned
                    b = spacer 240 40 |> color white |> opacity 0.85
                in
                    layers [b, (spacer 20 1) `beside` a `below` (spacer 1 10)] |> toForm |> move (140 - (toFloat w)/2, (toFloat h)/2 - 780)
        title = Html.span [style [("color", "blue"), ("font-size", "xx-large")]] [Html.text "GPS Visualization with ELM: 5 Vehicles in 24 Hours"] 
                |> Html.toElement 700 60 |> toForm |> move (380 - (toFloat w)/2,  (toFloat h)/2 - 40)
    in
        collage w h [toForm baseMap |> alpha malpha, fullTrace, vehicleTrace, info, title, anologClock_, digitClock_, progressBar_, vehicleStateView_, gitLink]

main : Signal Element
main = Signal.map4 render mapNetSg VideoControl.videoOptionSg dataSg vehicleOptionsSg
        