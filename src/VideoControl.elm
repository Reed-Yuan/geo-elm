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

import Widget
import MapControl exposing (..)

animationSg = 
    let
        anim startTime timeDelta = animation 0 |> from startTime |> to (startTime + (toFloat timeDelta) * 3600000) |> speed 400
    in
        Signal.map2 anim startTimeSg timeDeltaSg

global_t0 = Utils.timeFromString "2016-01-11T00:00:00"
global_t1 = Utils.timeFromString "2016-01-12T00:00:00"

type VideoStatus = Play | Pause | Stop
    
videoOps : Signal.Mailbox VideoStatus
videoOps = Signal.mailbox Play
        
realClock = 
    let
        tick (tDelta, videoStatus) state = 
            if videoStatus == Play
            then state + tDelta
            else if videoStatus == Stop
            then 0
            else state
    in
        Signal.foldp tick 0 (Signal.Extra.zip (Time.fps 25) videoOps.signal)

clock =
    let
        virtualClock t anime = animate t anime
    in
        Signal.map2 virtualClock realClock animationSg
        
videoRewindTaskSg = Signal.map2 (\t anime -> if isDone t anime then Signal.send videoOps.address Stop else Task.succeed ()) realClock animationSg
        
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
                p = (t - t0) / (toFloat td * 3600000) * 400 
                progress = segment (0,0) (p, 0) |> traced { defaultLine | width = 10, color = red } |> moveX -210
            in
                collage 440 10 [darkBar, progress]
                
        drawCtlAndPrgs videoStatus t t0 td = layers [spacer wth 30 |> color white |> opacity 0.85 
                       , spacer 40 10 `beside` (drawControls videoStatus) `beside` spacer 10 10 `beside` ((drawProgress t t0 td) `below` spacer 1 10)]        
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
        
startTimeSg = Signal.map (\( _, t) -> t) (fst startTimeCtlSg)
timeDeltaSg = Signal.map (\( _, t) -> t) (fst timeSpanCtlSg)
shadowSg = Signal.mergeMany [snd startTimeCtlSg, snd timeSpanCtlSg]

type alias VideoOptions =
    {
        time: Time,
        progressBar: Element,
        anologClock: Form,
        digitClock: Element,
        startTimeCtl: (Element, Float),
        timeDeltaCtl: (Element, Int)
    }

videoOptionSg =
    let
        videoSg_ = Signal.map5 VideoOptions clock videoControlSg analogClockSg digitalClockSg (fst startTimeCtlSg)
    in
        Signal.map2 (\x y -> x y) videoSg_ (fst timeSpanCtlSg)
        
        