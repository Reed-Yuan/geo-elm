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

videoControl t isPlaying =
    let
        ht = 40
        wth = 860
        icon_stop = FontAwesome.stop Color.darkGreen 20 |> Html.toElement 40 20 
                    |> Graphics.Input.clickable (Signal.message videoOps.address StopVideo)
        icon_play = FontAwesome.play Color.darkGreen 20 |> Html.toElement 40 20 
                    |> Graphics.Input.clickable (Signal.message videoOps.address PlayVideo)
        icon_pause = FontAwesome.pause Color.darkGreen 20 |> Html.toElement 40 20
                    |> Graphics.Input.clickable (Signal.message videoOps.address PauseVideo)
        icon_ = (if isPlaying then flow right [icon_pause, icon_stop] else flow right [icon_play, spacer 40 20]) |> toForm |> moveX -340
        darkBar = segment (0,0) (600, 0) |> traced { defaultLine | width = 10, color = darkGrey } |> moveX -200
        p = (t - global_t0) / (global_t1 - global_t0) * 600 
        progressBar = segment (0,0) (p, 0) |> traced { defaultLine | width = 10, color = red } |> moveX -200
    in 
        collage wth ht [
            rect (wth |> toFloat) (ht |> toFloat) |> filled Color.white |> alpha 0.7
            , darkBar, progressBar, icon_]

clockk t = 
    let
        face = filled lightGrey (circle 60) |> alpha 0.6
        outline_ = outlined ({ defaultLine | width = 4, color = Color.orange }) (circle 60)
        hand_mm = segment (0,0) (fromPolar (50, degrees (90 - 6 * inSeconds t/60))) |> traced { defaultLine | width = 5, color = blue }
        hand_hh = segment (0,0) (fromPolar (35, degrees (90 - 6 * inSeconds t/720))) |> traced { defaultLine | width = 8, color = green }
        clk = Graphics.Collage.group [face, outline_, hand_mm, hand_hh] |> moveX -20
        dTxt = Html.span [style [("padding", "4px 20px 4px 20px"), ("color", "blue"), ("font-size", "xx-large"), ("background-color", "rgba(255, 255, 255, 0.9)")]] 
                [Html.text (timeToString t)] |> Html.toElement 180 60
    in
       (clk, dTxt)

videoSg = 
    let
        aug state =
            let
                t = animate state.clock global_animation
                isPlaying = state.videoStatus == Playing
                progressBar = videoControl t isPlaying
                (anologClock, digitClock) = clockk t
            in
                (t, progressBar, anologClock, digitClock)
    in
        Signal.map aug videoStateSg 
        