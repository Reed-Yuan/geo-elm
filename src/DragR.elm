module DragR where

import Mouse
import Signal.Extra

type DragEvent
    = Start (Int, Int)
    | Move (Int, Int) (Int, Int)
    | End (Int, Int)
    | Nil
    
dragEvents: Signal Bool -> Signal Bool -> Signal DragEvent
dragEvents hoverSg enabledSg =
    let
        mouseChangesSg = Signal.Extra.zip Mouse.position Mouse.isDown |> Signal.Extra.deltas
        
        step ((((x0, y0), isDown0), ((x1, y1), isDown1)), inside, enabled) status =
            let
                d = 0 --Debug.log "(isDown0, isDown1)" (isDown0, isDown1)
            in
                if enabled then
                    case status of
                        Nil -> if (not isDown0) && isDown1 && inside then Start (x1, y1) else Nil
                        Start (xs, ys) -> if isDown0 && isDown1 && inside then Move (xs, ys) (x1, y1) else End (x0, y0)
                        Move (xs, ys) _ -> if isDown0 && isDown1 && inside then Move (xs, ys) (x1, y1) else End (x0, y0)
                        _ -> Nil
                else Nil
    in
        Signal.foldp step Nil (Signal.Extra.zip3 mouseChangesSg hoverSg enabledSg)
