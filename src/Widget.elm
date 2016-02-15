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
        
<<<<<<< HEAD
=======
        {-
        check (s, m, e) = 
            if not (s && e) then False
            else
                case m of
                    MoveFromTo _ _ -> True
                    _ -> False
                    
        filteredMouseEvt = Signal.Extra.zip3 hoverFlow.signal Drag.mouseEvents enabledSg 
                            |> Signal.filter check (False, StartAt (0,0), False) |> Signal.map (\(_, x, _) -> x)
        
>>>>>>> 503c31e0519d26649e359104c4e4b71acecd6914
        sliderOps : Signal Int
        sliderOps = 
            let
                op enabled inside msEvt =
                    if not (enabled && inside) then 0
                    else 
                        case msEvt of
                            MoveFromTo (x0,y0) (x1, y1) ->
                                if isVertical
                                then (y0 - y1)
                                else (x1 - x0)
                            _ -> 0
            in
<<<<<<< HEAD
                Signal.map3 op enabledSg hoverFlow.signal Drag.mouseEvents
=======
                Signal.map merge filteredMouseEvt
        -}
        
        sliderOps : Signal Int
        sliderOps = 
            let
                op enabled inside msEvt =
                    if not (enabled && inside) then 0
                    else 
                        case msEvt of
                            MoveFromTo (x0,y0) (x1, y1) ->
                                if isVertical
                                then (y0 - y1)
                                else (x1 - x0)
                            _ -> 0
            in
                Signal.map3 op enabledSg hoverFlow.signal Drag.mouseEvents
                    |> Signal.filter ((/=) 0) 0
>>>>>>> 503c31e0519d26649e359104c4e4b71acecd6914
                
        step a acc = (a + acc) |> Basics.min rPos |> Basics.max lPos
        posSignal = Signal.foldp step initPosition sliderOps |> Signal.dropRepeats
        
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
