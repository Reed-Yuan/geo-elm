module VehicleControl where

import Set exposing (..)
import Exts.Float
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Widget
import Set
import List.Extra
import Text
import String

type alias VehicleOptions =
    {
        traceAlpha: (Element, Float),
        tailLength: (Element, Int),
        mapAlpha: (Element, Float),
        selectedVehicles: Set Int
    }
        
traceAlphaSg = 
    let
        (sliderSg, shadowFlow) = Widget.slider "traceAlpha" 100 0.1 False (Signal.constant True)
        wrap (slider_, pct) = 
            let
                pct_ = Exts.Float.roundTo 2 pct
                title = Html.span [style [("padding-left", "10px"),("font-weight", "bold"),("font-size", "large")]] 
                        [Html.text ("Trace Fade: " ++ (toString pct_))] |> Html.toElement 160 30
                wrappedSlider = layers [spacer 20 1 `beside` slider_ `below` title]
            in
                (wrappedSlider, pct_)
    in
        (Signal.map wrap sliderSg, shadowFlow)

mapAlphaSg = 
    let
        (sliderSg, shadowFlow) = Widget.slider "mapAlpha" 100 0.5 False (Signal.constant True)
        wrap (slider_, pct) = 
            let
                pct_ = Exts.Float.roundTo 2 pct
                title = Html.span [style [("padding-left", "10px"),("font-weight", "bold"),("font-size", "large")]] 
                        [Html.text ("Map Fade: " ++ (toString pct_))] |> Html.toElement 160 30
                wrappedSlider = layers [spacer 20 1 `beside` slider_ `below` title]
            in
                (wrappedSlider, pct_)
    in
        (Signal.map wrap sliderSg, shadowFlow) 
        
tailSg = 
    let
        (sliderSg, shadowFlow) = Widget.slider "tailLength" 100 0.5 False (Signal.constant True)
        wrap (slider_, pct) = 
            let
                pct_ = (Exts.Float.roundTo 2 pct) * 120 |> round
                pct__ = pct_ - (pct_ % 5)
                title = Html.span [style [("padding-left", "10px"),("font-weight", "bold"),("font-size", "large")]] 
                        [Html.text ("Tail: " ++ (toString pct__) ++ " minutes")] |> Html.toElement 160 30
                wrappedSlider = layers [spacer 20 1 `beside` slider_ `below` title]
            in
                (wrappedSlider, pct__)
    in
        (Signal.map wrap sliderSg, shadowFlow) 

global_vehicleList = [2012347, 2017231, 2030413, 2036207, 2026201]
global_colors = [Color.red, Color.blue, Color.brown, Color.orange, Color.darkGreen]

checkMailboxes : List (Int, Signal.Mailbox Bool)
checkMailboxes = List.map (\i -> (i, Signal.mailbox True)) global_vehicleList

getCheckMailbox mboxs vId = 
    let
        r = List.Extra.find (\(id, _) -> id == vId) mboxs
    in
        case r of
            Just (_, mbox) -> mbox
            _ -> Signal.mailbox False

checkBoxes vlist =
    let
        id2Name colorr vid = vid - 2000000 |> toString |> String.append "#" |> Text.fromString |> Text.height 22 |> Text.color colorr
        addr vId = getCheckMailbox checkMailboxes vId |> .address
        box vId colorr = (Graphics.Input.checkbox (Signal.message (addr vId)) (Set.member vId vlist) |> (Graphics.Element.size 20 22) )
                    `beside` spacer 10 1 `beside` (vId |> (id2Name colorr) |> leftAligned) 
        cBoxList = List.map2 box global_vehicleList global_colors
        cBoxes = List.foldr (\a state -> a `above` state) Graphics.Element.empty cBoxList
        title = "Vehicles" |> Text.fromString |> Text.height 22 |> Text.bold |> leftAligned
    in
        spacer 1 10 `above` (spacer 10 1 `beside` title) `above` (spacer 1 20 `above` ((spacer 20 1) `beside` cBoxes))
        
vehicleListSg : Signal (Set Int)
vehicleListSg = 
    let
        foldStep (vid, mBox) sett = 
            let
                mapStep checked sett = if checked then Set.insert vid sett else sett
            in
                Signal.map2 mapStep mBox.signal sett
    in
        List.foldr foldStep (Signal.constant Set.empty) checkMailboxes
        
vehicleOptionsSg = Signal.map4 (\a b c d -> VehicleOptions a b c d) (fst traceAlphaSg) (fst tailSg) (fst mapAlphaSg) vehicleListSg
shadowSg = Signal.mergeMany [snd traceAlphaSg, snd tailSg, snd mapAlphaSg]
    
    