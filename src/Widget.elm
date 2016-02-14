module Widget where

import Graphics.Element exposing (..)
import List exposing (..)
import Color exposing (..)
import Mouse
import Drag exposing (..)
import Graphics.Input
import String
import Bitwise exposing (..)
import Signal.Extra
import FontAwesome
import Html exposing (..)
import Html.Events exposing (..)

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
        
        check (e, m) = 
            if not e then False
            else
                case m of
                   Just (MoveBy (_, _)) -> True
                   _ -> False
                    
        filteredMouseEvt = Drag.track False hoverFlow.signal |> Signal.Extra.zip enabledSg 
                            |> Signal.filter check (False, Nothing) |> Signal.map snd
        
        sliderOps : Signal Int
        sliderOps = 
            let
                merge msEvt =
                    case msEvt of
                        Just (MoveBy (dx, dy)) ->
                            if isVertical
                            then dy
                            else dx
                        _ -> 0
            in
                Signal.map merge filteredMouseEvt
        
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
            if isVertical
            then
                spacer knotHeight width
            else
                spacer width knotHeight
                
        hoverFlow: Signal.Mailbox Bool
        hoverFlow = Signal.mailbox False

        render x enabled = 
            let
                colorr = if enabled then Color.red else Color.darkGrey
                knot = (if isVertical 
                        then (Graphics.Element.spacer 20 10) 
                        else (Graphics.Element.spacer 10 20)) |> Graphics.Element.color colorr 
                     |> if isVertical 
                        then (container 20 width  (middleAt (absolute knotHeightHalf) (absolute (width - x)))) 
                        else (container width 20 (middleAt (absolute x) (absolute knotHeightHalf)))
                knotAndBar = layers [bar, knot, slideRect] |> Graphics.Input.hoverable (Signal.message hoverFlow.address)
                pct = ((x - knotWidthHalf) |> toFloat) / ((width - knotWidth) |> toFloat)
            in
                (knotAndBar, pct)
    in
        (Signal.map2 render posSignal enabledSg, hoverFlow.signal)
