module TileMap where 

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Color exposing (..)
import Time exposing (..)
import Bitwise
import Maybe
import Proj
import Html exposing (..)

type alias Map = 
    {
        size: (Int, Int),
        center: (Float, Float),
        zoom: Int
    }

type alias Gpsx = 
    {
        vehicleId: Int,
        vehicleName: String,
        lon: Float,
        lat: Float,
        timestamp: Time,
        speed: Float,
        direction: Float
    }
    
maxZoomLevel : Int
maxZoomLevel = 18

tileSize : Int
tileSize = 256

tileSizeHalf : Int
tileSizeHalf = half tileSize

half x = Bitwise.shiftRight x 1

isEven x = 0 == Bitwise.and x 1

originTiles (tx, ty) (xTiles, yTiles) = let
        shft lst = Maybe.withDefault 0 (List.head lst) |> flip (*) tileSize
        org x lst = shft lst + (List.length lst) * tileSizeHalf
    in (org tx xTiles, org ty yTiles)
    
initMap : (Int, Int) -> (Float, Float) -> Int -> Map    
initMap (width, height) (lat, lon) z = {size = (width, height), center = (lat, lon), zoom = z}

proj: (Float, Float) -> Map -> (Float, Float)
proj (lat, lon) mapp =
    let
        (mx, my) = Proj.latLonToMeters (fst mapp.center, snd mapp.center)
        (px, py) = Proj.metersToPixels (mx, my) mapp.zoom
        (mxv, myv) = Proj.latLonToMeters (lat, lon)
        (pxv, pyv) = Proj.metersToPixels (mxv, myv) mapp.zoom
    in
        (pxv - px |> toFloat, pyv - py |> toFloat)
        
path: List Gpsx -> Map -> Graphics.Collage.LineStyle -> Graphics.Collage.Form
path p mapp s = 
    let
        p_ = List.map (\g -> proj (g.lat, g.lon) mapp) p
    in
        Graphics.Collage.traced s (Graphics.Collage.path p_)
        
marker (lat, lon) mapp icn = 
    let
        (x, y) = proj (lat, lon) mapp
    in
        move (x, y) icn
    
point (lat, lon) mapp diameter color = 
    let
        (x, y) = proj (lat, lon) mapp
    in
        circle diameter |> filled color |> move (x, y)

loadMap : Map -> Element
loadMap mapi = let
        (width, height) = mapi.size
        (mx, my) = Proj.latLonToMeters (fst mapi.center, snd mapi.center)
        (px, py) = Proj.metersToPixels (mx, my) mapi.zoom
        (tx, ty) = Proj.pixelsToTile (px, py) |> flip Proj.googleTile mapi.zoom
        (xTiles, yTiles) = tiles (width, height) (tx, ty) mapi.zoom
        row dy =  map (\dx -> (singleTile dx dy mapi.zoom)) xTiles |> flow right
        rows = map row yTiles
        xoff = half width - ((px % tileSize) - tileSizeHalf)
        yoff = half height + ((py % tileSize) - tileSizeHalf)
    in flow down rows |> container width height (middleAt (absolute xoff) (absolute yoff))

numTiles span = span  - tileSize |> half |> toFloat |> (flip (/) (toFloat tileSize)) |> ceiling |> flip (+) 1
 
tiles (width, height) (tx, ty) zoom = let
        xn = numTiles width
        yn = numTiles height
        xTiles = List.map (\i -> tx + i) [-xn .. xn] |> List.map (\i -> i % (2 ^ zoom))
        yTiles = List.map (\i -> ty + i) [-yn .. yn]
    in (xTiles, yTiles)

singleTileImg x y zoom = 
    collage tileSize tileSize [
        singleTile x y zoom |> toForm,
        outlined (solid green) (rect (toFloat tileSize) (toFloat tileSize))]              

singleTile x y zoom = let
        url = "http://tile.openstreetmap.org/" ++ toString zoom 
            ++ "/" ++ toString x ++ "/" ++ toString y ++ ".png"
        maxi = 2 ^ zoom
    in 
        if x >= 0 && x < maxi && y >=0 && y < maxi
        then image tileSize tileSize url
        else spacer tileSize tileSize
        
moveToPx : Map -> (Int, Int) -> Map
moveToPx mapi (x, y) = let
        dx = mapi.size |> fst |> half |> (-) x
        dy = mapi.size |> snd |> half |> (-) y
    in panPx mapi (dx, dy)    

panPx : Map -> (Int, Int) -> Map
panPx mapi (dx, dy) = let
        (mxc, myc) = Proj.latLonToMeters (fst mapi.center, snd mapi.center)
        (pxc, pyc) = Proj.metersToPixels (mxc, myc) mapi.zoom
        pxc_ = pxc - dx --between (pxc + dx) 0 (256 * 2 ^ mapi.zoom - 1)
        pyc_ = between (pyc + dy) 0 (256 * 2 ^ mapi.zoom - 1)
        (mxc_, myc_) = Proj.pixelsToMeters (pxc_, pyc_) mapi.zoom
        (latc, lonc) = Proj.metersToLatLon (mxc_, myc_)
    in {mapi | center = (latc, lonc)}         

between x min_ max_  = max x min_ |> min max_
    
zoom : Map -> Int -> Map
zoom mapi n =
    if n == 0 then mapi
    else
        if n < 0 then
            if mapi.zoom == 0 then mapi
            else
                let 
                    n_ = -n
                    z = min n_ mapi.zoom
                in {mapi | zoom = mapi.zoom - z}
        else
            if mapi.zoom == maxZoomLevel then mapi
            else
                let 
                    z = maxZoomLevel - mapi.zoom |> min n 
                in {mapi | zoom = mapi.zoom + z}