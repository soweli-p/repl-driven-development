module Cubicle.Update
open Avalonia
open Avalonia.Input

open FSharpPlus.Lens


type GameState = unit

type Event = 
  | Tick
  | KeyPressed of Key
  | KeyReleased of Key


let initFn (): GameState = ()
let updateFn event state = state
