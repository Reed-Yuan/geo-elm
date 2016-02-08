module VehicleControl where

import Set exposing (..)
import Exts.Float
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetChecked)
import Widget

type VehicleOps = SelectVehicle Int Bool | Nil
    
vehicleOps : Signal.Mailbox VehicleOps
vehicleOps = Signal.mailbox Nil

traceAlphaHover: Signal.Mailbox Bool
traceAlphaHover = Signal.mailbox False

mapAlphaHover: Signal.Mailbox Bool
mapAlphaHover = Signal.mailbox False

tailLengthHover: Signal.Mailbox Bool
tailLengthHover = Signal.mailbox False

targetFlow = 
    let 
        merge traceAlpha mapAlpha tailLength = 
            if traceAlpha then "traceAlpha"
            else if mapAlpha then "mapAlpha"
            else if tailLength then "tailLength"
            else "nothing"
    in
        Signal.map3 merge traceAlphaHover.signal mapAlphaHover.signal tailLengthHover.signal
        
traceAlphaSg targetFlow = 
    let
        wrap (slider_, pct) = 
            let
                pct_ = Exts.Float.roundTo 2 pct
                title = Html.span [style [("padding-left", "10px"),("font-weight", "bold"),("font-size", "large")]] 
                        [Html.text ("Trace Alpha: " ++ (toString pct_))] |> Html.toElement 160 40
                slider = slider_ |> Graphics.Input.hoverable (Signal.message traceAlphaHover.address)
                wrappedSlider = layers [spacer 20 1 `beside` slider `below` title]
            in
                (wrappedSlider, pct_)
    in
        Signal.map wrap (Widget.slider "traceAlpha" 100 0 False targetFlow) 

mapAlphaSg targetFlow = 
    let
        wrap (slider_, pct) = 
            let
                pct_ = Exts.Float.roundTo 2 pct
                title = Html.span [style [("padding-left", "10px"),("font-weight", "bold"),("font-size", "large")]] 
                        [Html.text ("Map Alpha: " ++ (toString pct_))] |> Html.toElement 160 40
                slider = slider_ |> Graphics.Input.hoverable (Signal.message mapAlphaHover.address)
                wrappedSlider = layers [spacer 20 1 `beside` slider `below` title]
            in
                (wrappedSlider, pct_)
    in
        Signal.map wrap (Widget.slider "mapAlpha" 100 0.6 False targetFlow) 
        
tailSg targetFlow = 
    let
        wrap (slider_, pct) = 
            let
                pct_ = (Exts.Float.roundTo 2 pct) * 120 |> round
                pct__ = pct_ - (pct_ % 5)
                title = Html.span [style [("padding-left", "10px"),("font-weight", "bold"),("font-size", "large")]] 
                        [Html.text ("Tail: " ++ (toString pct__) ++ " minutes")] |> Html.toElement 160 40
                slider = slider_ |> Graphics.Input.hoverable (Signal.message tailLengthHover.address)
                wrappedSlider = layers [spacer 20 1 `beside` slider `below` title]
            in
                (wrappedSlider, pct__)
    in
        Signal.map wrap (Widget.slider "tailLength" 100 0.5 False targetFlow) 
       
checkBoxes vlist =
  (div [style [("padding-left", "10px")]] <|
        Html.span [style [("font-size", "x-large"), ("font-weight", "bold")]] [Html.text "Vehicles"]
        :: br [] []
        :: br [] []
        :: checkbox (Set.member 2012347 vlist) 2012347 "  #12347" "red"
        ++ checkbox (Set.member 2017231 vlist) 2017231 "  #17231" "blue"
        ++ checkbox (Set.member 2030413 vlist) 2030413 "  #30413" "brown"
        ++ checkbox (Set.member 2036207 vlist) 2036207 "  #36207" "orange"
        ++ checkbox (Set.member 2026201 vlist) 2026201 "  #26201" "darkGreen")
    |> (Html.toElement 160 200)
    
checkbox : Bool -> Int -> String -> String -> List Html
checkbox isChecked vid name colr =
  [ input
      [ type' "checkbox"
      , checked isChecked
      , on "change" targetChecked (Signal.message vehicleOps.address << (SelectVehicle vid))
      ]
      []
  , Html.span [style [("font-size", "x-large"), ("color", colr)]] [Html.text name]
  , br [] []
  ]

step : VehicleOps -> Set Int -> Set Int
step op lst = 
    case op of
        SelectVehicle id flag -> 
            let
                sel = if flag 
                        then
                            Set.insert id lst
                        else
                            Set.remove id lst
            in
                sel
        _ -> lst
        
vehicleListSg : Signal (Set Int)
vehicleListSg = 
    Signal.foldp step (Set.fromList [2012347, 2017231, 2030413, 2036207, 2026201]) vehicleOps.signal       
    
    