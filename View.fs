module Cubicle.View

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open Avalonia
open Avalonia.Controls
open Elmish

open Cubicle.Update


let viewFn (s: GameState) (d: Dispatch<Event>): IView = 
  Panel.create [
    Panel.background "#504945"
  ]
