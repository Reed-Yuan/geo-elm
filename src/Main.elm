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

import Data
import VideoControl
import VehicleControl exposing (..)

port vehicleIn : Signal (List (List (Int, String, Float, Float, Float, Float)))

port mouseWheelIn : Signal MouseWheel
port screenSizeIn : Signal (Int, Int)

global_colors : List Color
global_colors = [Color.red, Color.blue, Color.brown, Color.orange, Color.darkGreen]
global_icons : List (Color -> Int -> Html.Html)
global_icons = [FontAwesome.truck, FontAwesome.ambulance, FontAwesome.taxi, FontAwesome.motorcycle, FontAwesome.bus]

dataSg = Signal.map2 (\gps mapp -> List.map3 (\x y z -> Data.parseGps x y z mapp) gps global_colors global_icons) vehicleIn mapSg

render : TileMap.Map -> (Time, Element, Element) -> List Data.VehiclTrace -> (Element, Float) -> (Element, Float) -> (Element, Int) -> Set Int -> Element
render  mapp (t, progressBar, clockWidgt) data (traceAlpha, talpha) (mapAlpha, malpha) (tailLength, tl) vehicleList = 
    let
        w = mapp.size |> fst
        h = mapp.size |> snd
        baseMap = TileMap.loadMap mapp
        filteredTraces = List.filter (\(id_, _, _, _, _, _) -> Set.member id_ vehicleList) data
        clockWidgt_ = clockWidgt |> toForm |> move ((toFloat w)/2 - 80, (toFloat h)/2 - 160)
        progressBar_ = progressBar |> toForm |> move (0, 40 - (toFloat h)/2)
        vehicleTrace = List.map (\vtrace -> showTrace vtrace t tl mapp) filteredTraces |> Graphics.Collage.group
        fullTrace = List.map (\(_, _, _, _, vtrace, _) -> vtrace) filteredTraces |> Graphics.Collage.group |> alpha talpha
        bck = spacer 160 500 |> color white |> opacity 0.85
        checkBoxes_ = checkBoxes vehicleList
        vehicleStateView = layers [bck, mapAlpha `below` (spacer 1 30) `below` tailLength `below` (spacer 1 30) `below` traceAlpha `below` (spacer 1 30) `below` checkBoxes_]
        vehicleStateView_ = vehicleStateView |> Graphics.Input.hoverable (Signal.message shadowFlow.address) |> toForm |> move (140 - (toFloat w)/2, (toFloat h)/2 - 380)
        gitLink =
                let
                    a = Text.fromString "Source code @GitHub" |> Text.link "https://github.com/Reed-Yuan/geo-elm.git" |> Text.height 22 |> leftAligned
                    b = spacer 240 40 |> color white |> opacity 0.85
                in
                    layers [b, (spacer 20 1) `beside` a `below` (spacer 1 10)] |> toForm |> move (140 - (toFloat w)/2, (toFloat h)/2 - 780)
        title = Html.span [style [("color", "blue"), ("font-size", "xx-large")]] [Html.text "GPS Visualization with ELM: 5 Vehicles in 24 Hours"] 
                |> Html.toElement 700 60 |> toForm |> move (380 - (toFloat w)/2,  (toFloat h)/2 - 40)
    in
        collage w h [toForm baseMap |> alpha malpha, fullTrace, vehicleTrace, title, gitLink, clockWidgt_, progressBar_, vehicleStateView_]

showTrace: Data.VehiclTrace -> Time -> Int -> TileMap.Map -> Form
showTrace (_, vname, colr, icn, _, gps) t tcLength mapp = 
    let
        trace = List.filter (\g -> g.timestamp < t && t - g.timestamp <= (if tcLength == 0 then 24 else tcLength) * 60000) gps |> List.reverse
        head = case (List.head trace) of
            Just g -> 
                let
                    (x, y) = TileMap.proj (g.lat, g.lon) mapp
                    p = move (x, y) icn
                    n = vname |> Text.fromString 
                        |> outlinedText {defaultLine | width = 1, color = colr} 
                        |> move (x + 40, y + 20)
                in 
                    Graphics.Collage.group [p, n]
            _ -> Graphics.Element.empty  |> toForm
        hstE = 
            if tcLength == 0 then Graphics.Element.empty |> toForm
            else TileMap.path trace mapp {defaultLine | color = colr, width = 6, dashing=[8, 4]}
    in 
        Graphics.Collage.group [head, hstE]

main : Signal Element
main = Signal.map3 (\x y z -> x y z) (Signal.map5 render mapSg VideoControl.videoSg dataSg traceAlphaSg mapAlphaSg ) tailSg vehicleListSg

type alias MouseWheel = 
    {
        pos: (Int, Int),
        delta: Int
    }

type MapOps = Zoom Int | Pan (Int, Int) | Size (Int, Int) | NoOp

mapOps : Signal.Mailbox MapOps
mapOps = Signal.mailbox NoOp
shadowFlow: Signal.Mailbox Bool
shadowFlow = Signal.mailbox False

ops : Signal MapOps
ops = 
    let
        level x = if x < 0 then -1
                    else if x == 0 then 0
                    else 1
        zooms = (\ms -> ms.delta |> level |> Zoom) <~ mouseWheelIn
        sizing = (\(x, y) -> Size (x, y)) <~ screenSizeIn
        mouseDrag evt flag = 
            if flag then NoOp
            else
                case evt of
                    MoveFromTo (x0,y0) (x1, y1) -> Pan (x1 - x0, y1 - y0)
                    _ -> NoOp
        pan = Signal.map2 mouseDrag Drag.mouseEvents shadowFlow.signal
    in
        Signal.mergeMany [zooms, sizing, pan]
        
trans : MapOps -> TileMap.Map -> TileMap.Map
trans op mapp = 
    case op of
        Zoom z -> TileMap.zoom mapp z
        Pan (dx, dy) -> TileMap.panPx mapp (dx, dy)
        Size (x, y) -> {mapp | size = (x, y)}
        _ -> mapp

mapSg : Signal TileMap.Map        
mapSg = 
    let
        initMap = { size = (TileMap.tileSize, TileMap.tileSize), center = (43.83488, -79.5257), zoom = 13 }
    in
        Signal.foldp trans initMap ops
