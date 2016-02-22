module DragR where

import Mouse
import Signal.Extra

type DragEvent
    = Start (Int, Int)
    | Moved Int Int
    | End (Int, Int)
    | Nil
    
dragEvents: Signal Bool -> Signal Bool -> Signal DragEvent
dragEvents hoverSg enabledSg =
    let
        step ((x, y), isDown, inside, enabled) ((x0, y0), isDown0, status) =
            let
                status' =
                    if enabled then
                        case status of
                            Nil -> if (not isDown0) && isDown && inside then Start (x, y) else Nil
                            Start _ -> if isDown0 && isDown && inside then Moved (x - x0) (y - y0) else End (x, y)
                            Moved _ _-> if isDown0 && isDown && inside then Moved (x - x0) (y - y0) else End (x, y)
                            _ -> Nil
                    else Nil
            in
                ((x, y), isDown, status')
    in
        Signal.foldp step ((0, 0), False, Nil) (Signal.Extra.zip4 Mouse.position Mouse.isDown hoverSg enabledSg)
            |> Signal.map (\(_, _, z) -> z)
