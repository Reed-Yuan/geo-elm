module VideoControl where

import Time exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal.Extra exposing (..)
import Signal.Fun exposing (..)
import Html exposing (..)
import Text
import Color exposing (..)
import FontAwesome
import Graphics.Input
import Html.Attributes exposing (..)
import Utils exposing (..)
import Date 
import Date.Format
import Date.Config
import Date.Create
import Date.Config.Config_en_us exposing (..)
import Animation exposing (..)
import FontAwesome
import Exts.Float
import Mouse
import Task
import Signal
import Easing
import Bitwise
import Drag exposing (..)

import Widget
import MapControl exposing (..)

global_t0 = Utils.timeFromString "2016-01-11T00:00:00"
global_t1 = Utils.timeFromString "2016-01-12T00:00:00"

type VideoStatus = Play | Pause | Stop
    
videoOps : Signal.Mailbox VideoStatus
videoOps = Signal.mailbox Play
        
realClock = 
    let
        tick (tDelta, videoStatus) state =  
            (if videoStatus == Play
            then state + tDelta
            else if videoStatus == Stop
            then 0
            else state) 
    in
        Signal.foldp tick 0 (Signal.Extra.zip (Time.fps 20) videoOps.signal)
        
clock = 
    let
        tick =
            let
                step (clk, speedd, st0, td0, msEvt) (progress_, animation_, clockTag_, speed_) = 
                    let
                        progress = 
                            case msEvt of
                                Just (MoveBy (dx, _)) -> 
                                    Basics.max (progress_ + ((toFloat dx) / 800) * td0 * 3600000) st0
                                    |> Basics.min (st0 + 3600000 * td0)
                                _ -> if clk == 0 || progress_ == 0 then st0 else animate (clk - clockTag_) animation_

                        anime =
                           case msEvt of
                               Just _ -> animation_ |> from progress
                               _ -> (if clk == 0 || progress_ == 0
                                    then animation_ |> from st0 |> to (st0 + 3600000 * td0) |> speed speedd
                                    else if speedd == speed_
                                    then animation_
                                    else animation_ |> from progress |> speed speedd )
                                    
                        
                        clockTag = 
                            case msEvt of
                                Just _ -> clk
                                _ -> (if clk == 0 || speedd /= speed_
                                     then clk 
                                     else clockTag_)
                                
                    in
                        (progress, anime, clockTag, speedd)
            in
                Signal.foldp step (0, animation 0 |> ease Easing.linear, 0, 0 ) (Utils.zip5 realClock speedSg startTimeSg timeDeltaSg filteredMouseEvt)
    in
        tick |> Signal.map (\(x, y, z, w) -> x)

videoRewindTaskSg = Signal.map3 (\t1 t2 td -> if t1 >= t2 + 3600000 * td then Signal.send videoOps.address Stop else Task.succeed ()) clock startTimeSg timeDeltaSg

forwardFlow: Signal.Mailbox Bool
forwardFlow = Signal.mailbox False
filteredMouseEvt = Drag.track False forwardFlow.signal

videoControlSg =
    let
        wth = 580
        drawControls videoStatus = 
            let
                icon_stop = FontAwesome.stop Color.darkGreen 20 |> Html.toElement 40 20 
                            |> Graphics.Input.clickable (Signal.message videoOps.address Stop)
                icon_play = FontAwesome.play Color.darkGreen 20 |> Html.toElement 40 20 
                            |> Graphics.Input.clickable (Signal.message videoOps.address Play)
                icon_pause = FontAwesome.pause Color.darkGreen 20 |> Html.toElement 40 20
                            |> Graphics.Input.clickable (Signal.message videoOps.address Pause)
            in
                (if videoStatus == Play then icon_pause else icon_play) `beside` (if videoStatus == Stop then spacer 40 20 else icon_stop) `below` spacer 1 5
                
        drawProgress  t t0 td =
            let
                darkBar = segment (0,0) (400, 0) |> traced { defaultLine | width = 10, color = darkGrey } |> moveX -210
                p = (t - t0) / (td * 3600000) * 400 
                progress = segment (0,0) (p, 0) |> traced { defaultLine | width = 10, color = red } |> moveX -210
            in
                collage 440 10 [darkBar, progress] |> container 440 30 (topLeftAt (absolute 0) (absolute 10))
                    |> Graphics.Input.hoverable (Signal.message forwardFlow.address)
                
        drawCtlAndPrgs videoStatus t t0 td = layers [spacer wth 30 |> color white |> opacity 0.85 
                       , spacer 40 10 `beside` (drawControls videoStatus) `beside` spacer 10 10 `beside` (drawProgress t t0 td)]        
    in 
        Signal.map4 drawCtlAndPrgs videoOps.signal clock startTimeSg timeDeltaSg

analogClockSg = 
    let
        analogClock t = 
            let
                face = filled lightGrey (circle 60) |> alpha 0.6
                outline_ = outlined ({ defaultLine | width = 4, color = Color.orange }) (circle 60)
                hand_mm = segment (0,0) (fromPolar (50, degrees (90 - 6 * inSeconds t/60))) |> traced { defaultLine | width = 5, color = green }
                hand_hh = segment (0,0) (fromPolar (35, degrees (90 - 6 * inSeconds t/720))) |> traced { defaultLine | width = 8, color = blue }
            in
               Graphics.Collage.group [face, outline_, hand_mm, hand_hh]
    in
        Signal.map analogClock clock
        
digitalClockSg = 
    let
        digitalClock t = 
            let
                dTxt = timeToString t |> Text.fromString |> Text.height 35 |> Text.bold |> Text.color green |> leftAligned
            in
               layers [spacer 160 40 |> color white |> opacity 0.85, spacer 15 1 `beside` dTxt]
    in
        Signal.map digitalClock clock
        
startTimeCtlSg = 
    let
        (sliderSg, shadowFlow) = Widget.slider "startTime" 100 0.25 False (Signal.map ((==) Stop) videoOps.signal)
        f (slider_, pct) =
            let
                t =  ((pct * 23 |> round) * 3600000) |> toFloat |> (+) global_t0
                title = Html.span [style [("padding-left", "10px"),("font-weight", "bold"),("font-size", "large")]]
                            [Html.text ("From: " ++ timeToString t)] |> Html.toElement 160 30
                wrappedSlider = layers [spacer 20 1 `beside` slider_ `below` title]
            in
                (wrappedSlider, t)
    in
        (Signal.map f sliderSg, shadowFlow)

timeSpanCtlSg = 
    let
        (sliderSg, shadowFlow) = Widget.slider "timeDelta" 100 0.15 False (Signal.map ((==) Stop) videoOps.signal)
        f (slider_, pct) =
            let
                t =  pct * 23 |> round |> (+) 1
                title = Html.span [style [("padding-left", "10px"),("font-weight", "bold"),("font-size", "large")]] 
                            [Html.text ("Span: " ++ (toString t) ++ (if t == 1 then " hour" else " hours"))]|> Html.toElement 160 30
                wrappedSlider = layers [spacer 20 1 `beside` slider_ `below` title]
            in
                (wrappedSlider, t)
    in
        (Signal.map f sliderSg, shadowFlow)

speedCtlSg = 
    let
        (sliderSg, shadowFlow) = Widget.slider "timeDelta" 100 0.6 False (Signal.constant True)
        f (slider_, pct) =
            let
                t = pct * 8 |> round |> (+) 3 |> (Bitwise.shiftLeft) 1
                title = Html.span [style [("padding-left", "10px"),("font-weight", "bold"),("font-size", "large")]] 
                            [Html.text ("Play Speed: x " ++ (toString t))]|> Html.toElement 160 30
                wrappedSlider = layers [spacer 20 1 `beside` slider_ `below` title]
            in
                (wrappedSlider, t)
    in
        (Signal.map f sliderSg, shadowFlow)
        
startTimeSg = Signal.map (\( _, t) -> t) (fst startTimeCtlSg)
timeDeltaSg = Signal.map (\( _, t) -> toFloat t) (fst timeSpanCtlSg)
speedSg = Signal.map (\( _, t) -> toFloat t) (fst speedCtlSg)
shadowSg = Signal.mergeMany [snd startTimeCtlSg, snd timeSpanCtlSg, snd speedCtlSg, forwardFlow.signal]

type alias VideoOptions =
    {
        time: Time,
        progressBar: Element,
        anologClock: Form,
        digitClock: Element,
        startTimeCtl: (Element, Float),
        timeDeltaCtl: (Element, Int),
        speedCtl: (Element, Int)
    }

videoOptionSg =
    let
        videoSg_ = Signal.map5 VideoOptions clock videoControlSg analogClockSg digitalClockSg (fst startTimeCtlSg)
    in
        Signal.map3 (\x y z -> x y z) videoSg_ (fst timeSpanCtlSg) (fst speedCtlSg)
        
        