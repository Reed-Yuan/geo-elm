module Test where 

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

timeSpanCtlSg = 
    let
        (sliderSg, shadowFlow) = Widget.slider "timeDelta" 100 0.15 False (Signal.constant True)
        f (slider_, pct) =
            let
                t =  pct * 23 |> round |> (+) 1
                title = Html.span [style [("padding-left", "10px"),("font-weight", "bold"),("font-size", "large")]] 
                            [Html.text ("Span: " ++ (toString t) ++ (if t == 1 then " hour" else " hours"))]|> Html.toElement 160 30
                wrappedSlider = layers [spacer 20 1 `beside` slider_ `below` title]
            in
                (wrappedSlider, t)
    in
        Signal.map f sliderSg

main : Signal Element
main = Signal.map fst timeSpanCtlSg
        