module Main where 

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Color exposing (..)
import Time exposing (..)
import TileMap
import Bitwise
import Proj
import Char
import Keyboard
import Mouse
import Maybe exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetChecked)
import Text
import Drag exposing (..)
import Graphics.Input
import Time exposing (..)
import Animation exposing (..)
import Array
import Result exposing (..)
import String
import Signal.Extra exposing (..)
import Signal.Fun exposing (..)
import Date
import List.Extra exposing (..)
import Date.Format
import Date.Config
import Date.Create
import Date.Config.Config_en_us exposing (..)
import FontAwesome
import Set
import Exts.Float

type alias MouseWheel = 
    {
        pos: (Int, Int),
        delta: Int
    }

type alias VehiclTrace = (Int, String, Color, Form, List TileMap.Gpsx, List TileMap.Gpsx)
type VideoStatus = Playing | Pause | Stop

type alias State =
    {
        trace: List VehiclTrace,
        map: TileMap.Map,
        clock: Time,
        vehicleList: Set.Set Int,
        traceLength: Int,
        videoStatus: VideoStatus
    }

type Widget = Slider | BaseMap | Nil
    
type MapOps = Zoom Int | Pan (Int, Int) | Size (Int, Int) | NoOp | Tick Time | Buff (List VehiclTrace) | SelectVehicle Int Bool | TraceLength Int | PlayVideo | StopVideo | PauseVideo
type OpsLevel_0 = MoveSlider Float | NoOp_0
    
port mouseWheelIn : Signal MouseWheel
port screenSizeIn : Signal (Int, Int)
port vehicleIn : Signal (List (List (Int, String, Float, Float, Float, Float)))

global_tzone = Date.fromTime 0 |> Date.Create.getTimezoneOffset |> (*) 60000 |> (-) 0 |> toFloat

global_t0 = "2016-01-11 00:00:00" |> Date.fromString |> Result.withDefault (Date.fromTime 0) |> Date.toTime |> (+) global_tzone
global_t1 = "2016-01-12 00:00:00" |> Date.fromString |> Result.withDefault (Date.fromTime 0) |> Date.toTime |> (+) global_tzone

global_colors = [Color.red, Color.blue, Color.brown, Color.orange, Color.darkGreen]
global_icons = [FontAwesome.truck, FontAwesome.ambulance, FontAwesome.taxi, FontAwesome.motorcycle, FontAwesome.bus]

opFlow : Signal.Mailbox MapOps
opFlow = Signal.mailbox PlayVideo
shadowFlow: Signal.Mailbox Bool
shadowFlow = Signal.mailbox False

global_animation = animation 0 |> from global_t0 |> to global_t1 |> duration (240*Time.second)
        
slider widthHalf = 
    let
        widgetFlow: Signal.Mailbox Bool
        widgetFlow = Signal.mailbox False
        sliderOps : Signal Int
        sliderOps = 
            let
                merge flag msEvt =
                    if flag then
                        case msEvt of
                            MoveFromTo (x0,y0) (x1, y1) -> (toFloat (x1 - x0)) * 1.25 |> round
                            _ -> 0
                    else 0
            in
                Signal.map2 merge widgetFlow.signal Drag.mouseEvents
        
        sum_ a acc = (a + acc) |> Basics.min (widthHalf - 5) |> Basics.max (5 - widthHalf)
        pos = Signal.foldp sum_ 0 sliderOps
        rendr x = 
            let
                pct = ((x + widthHalf - 5) |> toFloat) / ((widthHalf - 5) |> toFloat) / 2 |> Exts.Float.roundTo 2
                title = Html.span [style [("font-weight", "bold"),("font-size", "x-large")]] [Html.text ("Map Alpha: " ++ (toString pct))] |> Html.toElement 200 40
                bar = Graphics.Element.spacer (widthHalf*2) 6 |> Graphics.Element.color Color.darkGrey 
                        |> container (2 * widthHalf) 20 (midLeftAt (absolute 0) (absolute 10))
                s = Graphics.Element.spacer 10 20 |> Graphics.Element.color Color.red |> Graphics.Input.hoverable (Signal.message widgetFlow.address)
                     |> container (2 * widthHalf) 20 (middleAt (absolute (x + widthHalf)) (absolute 10))
                bck = spacer 240 70 |> color white |> opacity 0.9
                c = spacer 30 1 `beside` layers [bar, s] `below` title
                v = layers [bck, c] |> Graphics.Input.hoverable (Signal.message shadowFlow.address) |> toForm
            in
                (v, pct)
    in
        Signal.map rendr pos  
        
ops : Signal MapOps
ops = 
    let
        level x = if x < 0 then -1
                    else if x == 0 then 0
                    else 1
        zooms = (\ms -> ms.delta |> level |> Zoom) <~ mouseWheelIn
        sizing = (\(x, y) -> Size (x, y)) <~ screenSizeIn
        hover = Signal.mailbox False
        mouseDrag evt flag = 
            if flag then NoOp
            else
                case evt of
                    MoveFromTo (x0,y0) (x1, y1) -> Pan (x1 - x0, y1 - y0)
                    _ -> NoOp
        pan = Signal.map2 mouseDrag Drag.mouseEvents shadowFlow.signal
        clock_ = Tick <~ Time.fps 25
        inject gps = List.map3 (\x y z -> parseGps x y z) gps global_colors global_icons |> Buff
        buff = inject <~ vehicleIn
    in
        Signal.mergeMany [zooms, sizing, pan, clock_, buff, opFlow.signal]

trans : MapOps -> State -> State
trans op state = 
    case op of
        Zoom z -> {state | map = TileMap.zoom state.map z}
        TraceLength i -> {state | traceLength = i}
        Pan (dx, dy) -> {state | map = TileMap.panPx state.map (dx, dy)}
        Size (x, y) -> let map_ = state.map in {state | map = {map_ | size = (x, y)}}
        Tick t -> 
            let
                isVideoDone = isDone state.clock global_animation
            in    
                if state.videoStatus == Playing && not isVideoDone
                    then {state | clock = state.clock + t}
                    else if isVideoDone then {state | videoStatus = Stop, clock = 0}
                    else state
        SelectVehicle i f ->
            let
                sel = if f 
                        then
                            Set.insert i state.vehicleList
                        else
                            Set.remove i state.vehicleList
            in
                {state | vehicleList = sel}
        Buff gps -> {state | trace = gps}
        PlayVideo -> {state | videoStatus = Playing}
        StopVideo -> {state | videoStatus = Stop, clock = 0}
        PauseVideo -> {state | videoStatus = Pause}
        _ -> state
        
states = 
    let
        initMap = { size = (TileMap.tileSize, TileMap.tileSize), center = (43.83488, -79.5257), zoom = 13 }
        initState = {trace = [], map = initMap, clock = 0, vehicleList=Set.fromList [2012347, 2017231, 2030413, 2036207, 2026201], traceLength = 1, videoStatus = Stop}
    in
        Signal.foldp trans initState ops
        
render : State -> (Form, Float) -> Element
render state (slid, alp) = 
    let
        w = state.map.size |> fst
        h = state.map.size |> snd
        baseMap = TileMap.loadMap state.map
        filteredTraces = List.filter (\(id_, _, _, _, _, _) -> Set.member id_ state.vehicleList) state.trace
        t = animate state.clock global_animation
        tc = List.map (\vtrace -> showTrace vtrace t state.traceLength state.map) filteredTraces |> Graphics.Collage.group
        isPlaying = state.videoStatus == Playing
        tbox = videoControl w 40 t isPlaying |> toForm |> move (0, 40 - (toFloat h / 2))
        cboxes = checkBoxes state.vehicleList |> Html.toElement 240 200 |> toForm |> move (140 - (toFloat w)/2, (toFloat h)/2 - 180)
        radios = traceLength state.traceLength |> Html.toElement 240 200 |> toForm |> move (140 - (toFloat w)/2, (toFloat h)/2 - 380)
        gitLink = Html.span [style [("padding", "4px 10px 4px 10px"),("background-color", "rgba(255, 255, 255, 0.9)"), 
                    ("color", "blue"), ("font-size", "x-large")]] [Html.text "Source code @GitHub"] 
                    |> Html.toElement 240 200 |> toForm |> move (140 - (toFloat w)/2, (toFloat h)/2 - 680)
        clockE = clocky t |> toForm  |> move ((toFloat w)/2 - 80, (toFloat h)/2 - 160)
        title = Html.span [style [("color", "blue"), ("font-size", "xx-large")]] [Html.text "Map Visualization with ELM: 5 Vehicles in 24 Hours"] 
                |> Html.toElement 700 60 |> toForm |> move (380 - (toFloat w)/2,  (toFloat h)/2 - 40)
        slidE = slid |> move (140 - (toFloat w)/2, (toFloat h)/2 - 510)        
    in
        --collage w h [toForm baseMap |> alpha alp, tbox, clockE, cboxes, radios, title, tc]
        collage w h [toForm baseMap |> alpha alp, tc, tbox, clockE, cboxes, radios, title, gitLink, slidE]
                    
showTrace: VehiclTrace -> Time -> Int -> TileMap.Map -> Form
showTrace (_, vname, colr, icn, _, gps) t tcLength mapp = 
    let
        trace = List.filter (\g -> g.timestamp < t && t - g.timestamp <= (if tcLength == 0 then 24 else tcLength) * 3600000) gps |> List.reverse
        head = case (List.head trace) of
            Just g -> 
                let
                    (x, y) = TileMap.proj (g.lat, g.lon) mapp
                    p = move (x, y) icn
                    n = vname |> Text.fromString 
                        |> outlinedText {defaultLine | width = 1, color = colr} 
                        |> move (x + 40, y + 20)
                in 
                    Graphics.Collage.group [p, n]
            _ -> Graphics.Element.empty  |> toForm
        hstE = 
            if tcLength == 0 then Graphics.Element.empty |> toForm
            else TileMap.path trace mapp {defaultLine | color = colr, width = 6, dashing=[8, 4]}
    in 
        Graphics.Collage.group [head, hstE]

main = Signal.map2 render states (slider 80)

videoControl width ht t isPlaying =
    let
        icon_stop = FontAwesome.stop Color.darkGreen 20 |> Html.toElement 40 20 
                    |> Graphics.Input.clickable (Signal.message opFlow.address StopVideo)
        icon_play = FontAwesome.play Color.darkGreen 20 |> Html.toElement 40 20 
                    |> Graphics.Input.clickable (Signal.message opFlow.address PlayVideo)
        icon_pause = FontAwesome.pause Color.darkGreen 20 |> Html.toElement 40 20
                    |> Graphics.Input.clickable (Signal.message opFlow.address PauseVideo)
        icon_ = (if isPlaying then flow right [icon_pause, icon_stop] else flow right [icon_play, spacer 40 20]) |> toForm |> moveX -340
        darkBar = segment (0,0) (600, 0) |> traced { defaultLine | width = 10, color = darkGrey } |> moveX -200
        p = (t - global_t0) / (global_t1 - global_t0) * 600 
        progressBar = segment (0,0) (p, 0) |> traced { defaultLine | width = 10, color = red } |> moveX -200
    in 
        collage width ht [
            rect (860 |> toFloat) (ht |> toFloat) 
                |> filled Color.white
                |> alpha 0.7
            , darkBar, progressBar, icon_]

parseGps : List (Int, String, Float, Float, Float, Float) -> Color -> (Color -> Int -> Html) -> VehiclTrace
parseGps gpsxRaw colr icn =
    let 
        parseRow (vid, timeStr, lat, lon, speed, direction) = 
            {vehicleId = vid
            , vehicleName = vid |> toString |> String.append "#"
            , timestamp = Date.fromString timeStr |> Result.withDefault (Date.fromTime 0) |> Date.toTime |> (+) global_tzone
            , lat = lat, lon = lon, speed = speed, direction = direction}
        isValidTime x = x.timestamp > 0
        isSameId g1 g2 = g1.vehicleId == g2.vehicleId
        process x = if List.isEmpty x then (-1, "", Color.red, Graphics.Element.empty  |> toForm, [], []) else
                        case List.head x of
                            Just gps -> (gps.vehicleId, gps.vehicleName, colr, (icn colr 24) |> Html.toElement 24 24 |> toForm, [], List.sortBy .timestamp x) 
                            _ -> (-1, "", Color.red, Graphics.Element.empty  |> toForm, [], [])
        gpsx = gpsxRaw |> List.map parseRow |> List.filter isValidTime |> process
    in
        gpsx

clocky t = 
    let
        face = filled lightGrey (circle 60) |> alpha 0.6
        outline_ = outlined ({ defaultLine | width = 4, color = Color.orange }) (circle 60)
        hand_mm = segment (0,0) (fromPolar (50, degrees (90 - 6 * inSeconds t/60))) |> traced { defaultLine | width = 5, color = blue }
        hand_hh = segment (0,0) (fromPolar (35, degrees (90 - 6 * inSeconds t/720))) |> traced { defaultLine | width = 8, color = green }
        clk = Graphics.Collage.group [face, outline_, hand_mm, hand_hh] |> moveX -20
        d = Date.fromTime (t - global_tzone) |> Date.Format.format Date.Config.Config_en_us.config "%H:%M:%S"
        dTxt = Html.span [style [("padding", "4px 20px 4px 20px"), ("color", "blue"), ("font-size", "xx-large"), ("background-color", "rgba(255, 255, 255, 0.9)")]] 
                [Html.text d] |> Html.toElement 180 60 |> toForm |> moveY -120
    in
        Graphics.Collage.collage 200 300 [clk, dTxt]  

checkBoxes vlist =
  div [style [("background-color", "rgba(255, 255, 255, 0.9)")]] <|
    Html.span [style [("font-size", "x-large"), ("font-weight", "bold")]] [Html.text "Select vehicles"]
    :: br [] []
    :: br [] []
    :: checkbox (Set.member 2012347 vlist) 2012347 "  #2012347" "red"
    ++ checkbox (Set.member 2017231 vlist) 2017231 "  #2017231" "blue"
    ++ checkbox (Set.member 2030413 vlist) 2030413 "  #2030413" "brown"
    ++ checkbox (Set.member 2036207 vlist) 2036207 "  #2036207" "orange"
    ++ checkbox (Set.member 2026201 vlist) 2026201 "  #2026201" "darkGreen"
    
checkbox : Bool -> Int -> String -> String -> List Html
checkbox isChecked vid name colr =
  [ input
      [ type' "checkbox"
      , checked isChecked
      , on "change" targetChecked (Signal.message opFlow.address << (SelectVehicle vid))
      ]
      []
  , Html.span [style [("font-size", "x-large"), ("color", colr)]] [Html.text name]
  , br [] []
  ]

traceLength cur =
  div [style [("background-color", "rgba(255, 255, 255, 0.9)")]] <|
    Html.span [style [("font-size", "x-large"), ("font-weight", "bold")]] [Html.text "Select Trace Length"]
    :: br [] []
    :: br [] []
    :: radio cur 0 "  No trace"
    ++ radio cur 1 "  1 hour trace"
    ++ radio cur 2 "  2 hours trace"
    ++ radio cur 8 "  8 hours trace"
    ++ radio cur 24 "  24 hours trace"

radio : Int -> Int -> String -> List Html
radio cur i name =
  [ input
      [ type' "radio"
      , checked (cur == i)
      , on "change" targetChecked (\_ -> Signal.message opFlow.address (TraceLength i))
      ]
      []
  , Html.span [style [("font-size", "x-large")]] [Html.text name]
  , br [] []
  ]
  