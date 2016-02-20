module DragR where

import Mouse
import Signal.Extra

type DragEvent
    = StartAt (Int, Int)
    | MoveFromTo (Int, Int) (Int, Int)
    | EndAt (Int, Int)
    | Nil
    
dragEvents: Signal Bool -> Signal Bool -> Signal DragEvent
dragEvents hoverSg enabledSg =
    let
        mouseChangesSg = Signal.Extra.zip Mouse.position Mouse.isDown |> Signal.Extra.deltas
        
        step ((((x0, y0), isDown0), ((x1, y1), isDown1)), inside, enabled) status =
                if enabled then
                    case status of
                        Nil -> if (not isDown0) && isDown1 && inside then StartAt (x1, y1) else Nil
                        StartAt (_, _) -> if isDown0 && isDown1 && inside then MoveFromTo (x0, y0) (x1, y1) else EndAt (x1, y1)
                        MoveFromTo _ _ -> if isDown0 && isDown1 && inside then MoveFromTo (x0, y0) (x1, y1) else EndAt (x1, y1)
                        _ -> Nil
                else Nil
    in
        Signal.foldp step Nil (Signal.Extra.zip3 mouseChangesSg hoverSg enabledSg)
