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

import Widget
import MapControl exposing (..)

global_animation = animation 0 |> from global_t0 |> to global_t1 |> duration (240*Time.second)

global_t0 = Utils.timeFromString "2016-01-11T00:00:00"
global_t1 = Utils.timeFromString "2016-01-12T00:00:00"

type VideoStatus = Playing | Pause | Stop

type alias State =
    {
        clock: Time,
        videoStatus: VideoStatus
    }
    
type VideoOps = Tick Time | PlayVideo | StopVideo | PauseVideo

videoOps : Signal.Mailbox VideoOps
videoOps = Signal.mailbox PlayVideo

ops : Signal VideoOps
ops = 
    let
        clock_ = Tick <~ Time.fps 25
    in
        Signal.mergeMany [clock_, videoOps.signal]

trans : VideoOps -> State -> State
trans op state = 
    case op of
        Tick t -> 
            let
                isVideoDone = isDone state.clock global_animation
            in    
                if state.videoStatus == Playing && not isVideoDone
                    then {state | clock = state.clock + t}
                    else if isVideoDone then {state | videoStatus = Stop, clock = 0}
                    else state
        PlayVideo -> {state | videoStatus = Playing}
        StopVideo -> {state | videoStatus = Stop, clock = 0}
        PauseVideo -> {state | videoStatus = Pause}

videoStateSg = Signal.foldp trans (State 0 Playing) ops

videoControl t videoStatus (startTime, iconnA, sliderA, t0) (timeDelta, iconnB, sliderB, tDelta) =
    let
        ht = 40
        wth = 820
        isPlaying = videoStatus == Playing
        icon_stop = FontAwesome.stop Color.darkGreen 20 |> Html.toElement 40 20 
                    |> Graphics.Input.clickable (Signal.message videoOps.address StopVideo)
        icon_play = FontAwesome.play Color.darkGreen 20 |> Html.toElement 40 20 
                    |> Graphics.Input.clickable (Signal.message videoOps.address PlayVideo)
        icon_pause = FontAwesome.pause Color.darkGreen 20 |> Html.toElement 40 20
                    |> Graphics.Input.clickable (Signal.message videoOps.address PauseVideo)
        icon_ = (if isPlaying then icon_pause else icon_play) `beside` (if videoStatus == Stop then spacer 40 20 else icon_stop)
        darkBar = segment (0,0) (400, 0) |> traced { defaultLine | width = 10, color = darkGrey } |> moveX -210
        p = (t - global_t0) / (global_t1 - global_t0) * 400 
        progress = segment (0,0) (p, 0) |> traced { defaultLine | width = 10, color = red } |> moveX -210
        progressBar = layers [spacer 440 30 |> color white |> opacity 0.85, spacer 1 10 `above` collage 440 10 [darkBar, progress]]
        editIcon_1 = if (videoStatus /= Stop) then spacer 24 1 else iconnA
        editIcon_2 = if (videoStatus /= Stop) then spacer 24 1 else iconnB
        ctls = layers [spacer wth ht |> color white |> opacity 0.85,
                spacer 20 1 `beside` startTime `beside` editIcon_1 `beside` spacer 10 1 `beside` progressBar 
                `beside` timeDelta  `beside` editIcon_2 `beside` spacer 20 1 `beside` icon_ `below` spacer 1 10]
                |> container wth 40 (bottomLeftAt (absolute 0) (absolute 0))
        sliders = spacer 110 1 `beside` sliderA `beside` spacer 530 1 `beside` sliderB
                |> container wth 160 (bottomLeftAt (absolute 0) (absolute 0))
    in 
        sliders `above` spacer wth 10 `above` ctls

clockk t = 
    let
        face = filled lightGrey (circle 60) |> alpha 0.6
        outline_ = outlined ({ defaultLine | width = 4, color = Color.orange }) (circle 60)
        hand_mm = segment (0,0) (fromPolar (50, degrees (90 - 6 * inSeconds t/60))) |> traced { defaultLine | width = 5, color = blue }
        hand_hh = segment (0,0) (fromPolar (35, degrees (90 - 6 * inSeconds t/720))) |> traced { defaultLine | width = 8, color = green }
        clk = Graphics.Collage.group [face, outline_, hand_mm, hand_hh] |> moveX -20
        dTxt = Html.span [style [("padding", "4px 22px 4px 22px"), ("color", "blue"), ("font-size", "xx-large"), ("background-color", "rgba(255, 255, 255, 0.9)")]] 
                [Html.text (timeToString t)] |> Html.toElement 160 60
    in
       (clk, dTxt)

videoSg = 
    let
        aug state (startTime, iconnA, sliderA, t0) (timeSpan, iconnB, sliderB, tDelta) =
            let
                t = animate state.clock global_animation
                progressBar = videoControl t state.videoStatus (startTime, iconnA, sliderA, t0) (timeSpan, iconnB, sliderB, tDelta)
                (anologClock, digitClock) = clockk t
            in
                (t, progressBar, anologClock, digitClock)
    in
        Signal.map3 aug videoStateSg startTimeCtlSg timeSpanCtlSg 

startTimeHover: Signal.Mailbox Bool
startTimeHover = Signal.mailbox False

timeDeltaHover: Signal.Mailbox Bool
timeDeltaHover = Signal.mailbox False

targetFlow = 
    let 
        merge startTime timeDelta = 
            if startTime then "startTime"
            else if timeDelta then "timeDelta"
            else "nothing"
    in
        Signal.map2 merge startTimeHover.signal timeDeltaHover.signal

startTimeCtlSg = 
    let
        f (iconn, sliderr, pct) =
            let
                t =  ((pct * 23 |> round) * 3600000) |> toFloat |> (+) global_t0
                timeNode = Html.span 
                            [style [("font-size", "large"), ("font-weight", "bold"), ("color", "blue")]] 
                            [Html.text (timeToString t)] |> (Html.toElement 100 30)
            in
                ( timeNode, iconn, sliderr, t)
    in
        Signal.map f (clickSlider "startTime" 100 0 True)

timeSpanCtlSg = 
    let
        f (iconn, sliderr, pct) =
            let
                t =  pct * 23 |> round |> (+) 1
                timeNode = Html.span 
                            [style [("font-size", "large"), ("font-weight", "bold"), ("color", "blue")]] 
                            [Html.text ("+ " ++ (toString t) ++ " hours")] |> (Html.toElement 100 30)
            in
                ( timeNode, iconn, sliderr, t)
    in
        Signal.map f (clickSlider "timeDelta" 100 0 True)

clickFlow : Signal.Mailbox String
clickFlow = Signal.mailbox ""

clickSlider : String -> Int -> Float -> Bool -> Signal (Element, Element, Float)
clickSlider name width initValue isVertical = 
    let
        editStateSg_ : Signal Bool
        editStateSg_ = Signal.foldp (\f state -> if f then not state else state ) False ((\s -> s == name) <~ clickFlow.signal)
        editStateSg = Signal.map2 (\s x -> if x == PlayVideo then False else s) editStateSg_ videoOps.signal
        editIcon = (FontAwesome.pencil Color.darkGreen 24) |> Html.toElement 24 24 |> Graphics.Input.clickable (Signal.message clickFlow.address name)
        dock isEditing (slider, pct) = 
            let
                sliderBar = (layers [spacer 40 120 |> color white |> opacity 0.85, spacer 10 1 `beside` slider `below` spacer 1 10])
            in
                if isEditing
                then (editIcon, sliderBar, pct)
                else (editIcon, spacer 40 0, pct)
    in
        Signal.map2 dock editStateSg (Widget.slider name width initValue isVertical)