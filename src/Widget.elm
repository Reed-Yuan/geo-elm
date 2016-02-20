module Widget where

import Graphics.Element exposing (..)
import List exposing (..)
import Color exposing (..)
import Mouse
import Graphics.Input
import String
import Bitwise exposing (..)
import Signal.Extra
import FontAwesome
import Html exposing (..)
import Html.Events exposing (..)
import DragR exposing (..)

slider : String -> Int -> Float -> Bool -> Signal Bool ->(Signal (Element, Float), Signal Bool)
slider name width initValue isVertical enabledSg = 
    let
        knotHeight = 20
        knotWidth = 10
        barHeight = 6
        widthHalf = width `shiftRight` 1
        knotHeightHalf = knotHeight `shiftRight` 1
        knotWidthHalf = knotWidth `shiftRight` 1
        lPos = knotWidthHalf
        rPos = width - knotWidthHalf
        initPosition = ((initValue * (toFloat (width - knotWidth))) |> round) + knotWidthHalf
        
        sliderStep msEvt (pos0, pos) = 
            let
                pos0' = case msEvt of
                    Start _ -> pos
                    _ -> pos0
                newPos = case msEvt of
                    Move (x0, y0) (x1, y1) ->
                        (if isVertical
                            then pos0 + y1 - y0
                            else pos0 + x1 - x0) 
                        |> Basics.min rPos |> Basics.max lPos
                    _ -> pos
                d = Debug.log "(pos0, msEvt, newPos)" (pos0, msEvt, newPos)
            in
                (pos0', newPos)
                
        posSignal = Signal.foldp sliderStep (initPosition, initPosition) (dragEvents hoverFlow.signal enabledSg) 
                    |> Signal.map snd |> Signal.dropRepeats
        
        slideRect = (if isVertical then spacer (knotHeight + 10) (width + 20) else spacer (width + 20) (knotHeight + 10))
                    |> Graphics.Input.hoverable (Signal.message hoverFlow.address)

        vertView pos colorr = 
            let
                bar = Graphics.Element.spacer barHeight width |> Graphics.Element.color Color.darkGrey 
                        |> container knotHeight width (midTopAt (absolute knotHeightHalf) (absolute 0))
                knot = (Graphics.Element.spacer 20 10) |> Graphics.Element.color colorr
                        |> (container 20 width  (middleAt (absolute knotHeightHalf) (absolute (width - pos))))
            in
                layers [bar, knot, slideRect]
        
        horizView pos colorr = 
            let
                bar = Graphics.Element.spacer width barHeight |> Graphics.Element.color Color.darkGrey 
                        |> container width knotHeight (midLeftAt (absolute 0) (absolute knotHeightHalf))
                knot = (Graphics.Element.spacer 10 20) |> Graphics.Element.color colorr
                        |> (container width 20 (middleAt (absolute pos) (absolute knotHeightHalf)))
            in
                spacer 10 1 `beside` layers [bar, knot] `below` spacer 1 5 |> \x -> layers [x, slideRect]
        
        hoverFlow: Signal.Mailbox Bool
        hoverFlow = Signal.mailbox False

        render x enabled = 
            let
                colorr = if enabled then Color.red else Color.darkGrey
                view = if isVertical then (vertView x colorr) else (horizView x colorr) 
                pct = ((x - knotWidthHalf) |> toFloat) / ((width - knotWidth) |> toFloat)
            in
                (view, pct)
    in
        (Signal.map2 render posSignal enabledSg, hoverFlow.signal)
