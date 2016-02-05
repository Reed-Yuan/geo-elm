module Widget where

import Graphics.Element exposing (..)
import List exposing (..)
import Color exposing (..)
import Mouse
import Drag exposing (..)
import Graphics.Input
import String
import Bitwise exposing (..)

slider : Int -> Float -> Bool ->Signal (Element, Float)
slider width initValue isVertical = 
    let
        knotHeight = 20
        knotWidth = 10
        barHeight = 6
        widthHalf = width `shiftRight` 1
        knotHeightHalf = knotHeight `shiftRight` 1
        knotWidthHalf = knotWidth `shiftRight` 1
        lPos = knotWidthHalf
        rPos = width - knotWidthHalf
        widgetFlow: Signal.Mailbox Bool
        widgetFlow = Signal.mailbox False
        initPosition = ((initValue * (toFloat (width - knotWidth))) |> round) + knotWidthHalf
        sliderOps : Signal Int
        sliderOps = 
            let
                merge flag msEvt =
                    if flag then
                        case msEvt of
                            MoveFromTo (x0,y0) (x1, y1) ->
                                if isVertical
                                then (y0 - y1)
                                else (x1 - x0)
                            _ -> 0
                    else 0
            in
                Signal.map2 merge widgetFlow.signal Drag.mouseEvents
        
        step a acc = (a + acc) |> Basics.min rPos |> Basics.max lPos
        posSignal = Signal.foldp step initPosition sliderOps
        bar = 
            if isVertical
                then
                    Graphics.Element.spacer barHeight width |> Graphics.Element.color Color.darkGrey 
                        |> container knotHeight width (midTopAt (absolute knotHeightHalf) (absolute 0)) 
                else
                    Graphics.Element.spacer width barHeight |> Graphics.Element.color Color.darkGrey 
                        |> container width knotHeight (midLeftAt (absolute 0) (absolute knotHeightHalf))
        slideRect = 
            (if isVertical
                then
                    spacer knotHeight width 
                else
                    spacer width knotHeight) |> Graphics.Input.hoverable (Signal.message widgetFlow.address)
        render x = 
            let
                knot = (if isVertical 
                        then (Graphics.Element.spacer 20 10) 
                        else (Graphics.Element.spacer 10 20)) |> Graphics.Element.color Color.red 
                     |> if isVertical 
                        then (container 20 width  (middleAt (absolute knotHeightHalf) (absolute (width - x)))) 
                        else (container width 20 (middleAt (absolute x) (absolute knotHeightHalf)))
                knotAndBar = layers [bar, knot, slideRect]
                pct = ((x - knotWidthHalf) |> toFloat) / ((width - knotWidth) |> toFloat)
            in
                (knotAndBar, pct)
    in
        Signal.map render posSignal  
