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
import Task exposing (..)
import String

import Data exposing (..)
import MapControl exposing (..)
import VideoControl exposing (..)
import VehicleControl exposing (..)
import Widget

port vehicleIn : Signal (List (List (Int, String, Float, Float, Float, Float)))

port mouseWheelIn : Signal MouseWheel
port screenSizeIn : Signal (Int, Int)
port browserIn : Signal String

port runner : Signal (Task x ())
port runner = VideoControl.videoRewindTaskSg
  
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

hideVehiclesMbx : Signal.Mailbox ()
hideVehiclesMbx = Signal.mailbox ()

hideInfoMbx : Signal.Mailbox ()
hideInfoMbx = Signal.mailbox ()

showWarnMbx : Signal.Mailbox Bool
showWarnMbx = Signal.mailbox True

hideCtlSg =
    let
        hideVehiclesSg = Signal.foldp (\_ b -> not b) False hideVehiclesMbx.signal            
        hideInfoSg = Signal.foldp (\_ b -> not b) False hideInfoMbx.signal
    in
        Signal.Extra.zip hideVehiclesSg hideInfoSg
    
render : TileMap.Map -> VideoOptions -> List Data.VehiclTrace -> VehicleOptions -> (Bool, Bool) -> Bool -> String -> Element
render  mapp videoOptions data vehicleOptions (hideVehicles, hideInfo) showWarn browserType = 
    let
        w = mapp.size |> fst
        h = mapp.size |> snd
        
        popA =
            let
                warn =  "Your browser is " 
                            ++ browserType ++ ", some controls in \nthis demo may not be working properly, \nplease use Google Chrome for best effects !!"
                            |> Text.fromString |> Text.color yellow |> Text.height 20 |> leftAligned
                warnButton = spacer 180 1 `beside` (Graphics.Input.button (Signal.message showWarnMbx.address False) "OK"  |> Graphics.Element.size 40 24) `below` spacer 1 20
                popp = layers [spacer 440 150 |> color black |> opacity 0.5, (spacer 40 1 `beside` warn `below` spacer 1 20) `above` warnButton] 
                        |> toForm |> move (420 - (toFloat w)/2,  (toFloat h)/2 - 140)
            in
                if showWarn && not (String.startsWith "Chrome" browserType) then popp else (Graphics.Element.empty |> toForm)
        
        (traceAlpha, talpha) = vehicleOptions.traceAlpha
        (mapAlpha, malpha) = vehicleOptions.mapAlpha
        (tailLength, tl) = vehicleOptions.tailLength
        vehicleList = vehicleOptions.selectedVehicles

        baseMap = TileMap.loadMap mapp
        filteredTraces = List.filter (\(id_, _, _, _, _, _) -> Set.member id_ vehicleList) data
        anologClock_ = videoOptions.anologClock |> move ((toFloat w)/2 - 280, (toFloat h)/2 - 70)
        digitClock_ = videoOptions.digitClock |> toForm |> move ((toFloat w)/2 - 100, (toFloat h)/2 - 50)
        progressBar_ = videoOptions.progressBar |> toForm |> move (0, 40 - (toFloat h)/2)
        
        traceWithInfo = List.map (\vtrace -> showTrace vtrace videoOptions.time tl mapp) filteredTraces |> List.unzip
        vehicleTrace = fst traceWithInfo |> group
        fullTrace = List.map (\(_, _, _, _, vtrace, _) -> vtrace) filteredTraces |> group |> alpha talpha
        
        vehicleInfo =
            let
                icn = (if hideInfo then FontAwesome.arrow_down white 20 else FontAwesome.arrow_up white 20) |> Html.toElement 20 20
                switch = layers [spacer 160 20 |> color grey |> Graphics.Element.opacity 0.5, spacer 70 20 `beside` icn]  
                             |> Graphics.Input.clickable (Signal.message hideInfoMbx.address ())
                info_ = (snd traceWithInfo) |> (List.foldr above Graphics.Element.empty) 
                        |> container 160 800 (midTopAt (absolute 80) (absolute 0))
                        
                view = (if hideInfo then switch else switch `above` info_)
           in
                view |> toForm |> move ((toFloat w)/2 - 100, if hideInfo then 380 else -20)
        
        vehicleStateView_ = 
            let
                bck = spacer 160 640 |> color white |> opacity 0.85
                checkBoxes_ = checkBoxes vehicleList
                vehicleStateView = layers [bck, 
                                    mapAlpha `below` (spacer 1 20) 
                                    `below` tailLength `below` (spacer 1 20) 
                                    `below` traceAlpha `below` (spacer 1 20)
                                    `below` (fst videoOptions.speedCtl) `below` (spacer 1 20)
                                    `below` (fst videoOptions.timeDeltaCtl) `below` (spacer 1 20)
                                    `below` (fst videoOptions.startTimeCtl) `below` (spacer 1 30)
                                    `below` checkBoxes_]
                icn = (if hideVehicles then FontAwesome.arrow_down white 20 else FontAwesome.arrow_up white 20) |> Html.toElement 20 20
                switch = layers [spacer 160 20 |> color grey |> Graphics.Element.opacity 0.5, spacer 70 20 `beside` icn]  
                             |> Graphics.Input.clickable (Signal.message hideVehiclesMbx.address ())
                view = (if hideVehicles then switch else switch `above` vehicleStateView)
            in 
                view |> toForm |> move (100 - (toFloat w)/2, if hideVehicles then 360 else 40)
                
        gitLink =
                let
                    a = Text.fromString "Source code @GitHub" |> Text.link "https://github.com/Reed-Yuan/geo-elm.git" |> Text.height 22 |> leftAligned
                    b = spacer 240 40 |> color white |> opacity 0.85
                in
                    layers [b, (spacer 20 1) `beside` a `below` (spacer 1 10)] |> toForm |> move (140 - (toFloat w)/2, 45 - (toFloat h)/2)
                    
        title = Html.span [style [("color", "blue"), ("font-size", "xx-large")]] [Html.text "GPS Visualization with ELM: 5 Vehicles in 24 Hours"] 
                |> Html.toElement 700 60 |> toForm |> move (380 - (toFloat w)/2,  (toFloat h)/2 - 40)
    in
        collage w h [toForm baseMap |> alpha malpha, fullTrace, vehicleTrace,  title, anologClock_, digitClock_, popA, progressBar_, vehicleStateView_, vehicleInfo,gitLink]

main : Signal Element
main = Signal.map3 (\x y z -> x y z) (Signal.map5 render mapNetSg VideoControl.videoOptionSg dataSg vehicleOptionsSg hideCtlSg) showWarnMbx.signal browserIn
        