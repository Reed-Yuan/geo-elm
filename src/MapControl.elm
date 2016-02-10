module MapControl where

import TileMap
import Signal.Extra exposing (..)
import Drag exposing (..)

type alias MouseWheel = 
    {
        pos: (Int, Int),
        delta: Int
    }

type MapOps = Zoom Int | Pan (Int, Int) | Size (Int, Int) | NoOp

mapOps : Signal.Mailbox MapOps
mapOps = Signal.mailbox NoOp

ops : Signal MouseWheel -> Signal (Int, Int) -> Signal Drag.MouseEvent -> Signal MapOps
ops mouseWheelIn screenSizeIn filteredMouseEvt = 
    let
        level x = if x < 0 then -1
                    else if x == 0 then 0
                    else 1
        zooms = (\ms -> ms.delta |> level |> Zoom) <~ mouseWheelIn
        sizing = (\(x, y) -> Size (x, y)) <~ screenSizeIn
        mouseDrag evt = 
            case evt of
                MoveFromTo (x0,y0) (x1, y1) -> Pan (x1 - x0, y1 - y0)
                _ -> NoOp
        pan = Signal.map mouseDrag filteredMouseEvt
    in
        Signal.mergeMany [zooms, sizing, pan]
        
trans : MapOps -> TileMap.Map -> TileMap.Map
trans op mapp = 
    case op of
        Zoom z -> TileMap.zoom mapp z
        Pan (dx, dy) -> TileMap.panPx mapp (dx, dy)
        Size (x, y) -> {mapp | size = (x, y)}
        _ -> mapp

mapSg : Signal MouseWheel -> Signal (Int, Int) -> Signal Drag.MouseEvent -> Signal TileMap.Map        
mapSg mouseWheelIn screenSizeIn filteredMouseEvt = 
    let
        initMap = { size = (TileMap.tileSize, TileMap.tileSize), center = (43.83488, -79.5257), zoom = 13 }
    in
        Signal.foldp trans initMap (ops mouseWheelIn screenSizeIn filteredMouseEvt)
