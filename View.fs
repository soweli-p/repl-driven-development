module Cubicle.View

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open Avalonia.Controls
open Elmish

open Cubicle.Update
open Avalonia


let playerToPoints (resolution: Vector) (p: Player) = 
  let newPos = new Point(p.Position.X, resolution.Y - p.Position.Y)
  [
    newPos
    new Point(newPos.X, newPos.Y + playerSize.Y)
    new Point(newPos.X + playerSize.Y, newPos.Y + playerSize.Y)
    new Point(newPos.X + playerSize.Y, newPos.Y)
  ]

let drawPlayer res player: IView = 
  Polygon.create [
    let points = playerToPoints res player
    Shapes.Polygon.points points
    Shapes.Polygon.fill "#fe8019"
  ]
let viewFn resolution (s: GameState) (d: Dispatch<Event>): IView = 
  Panel.create [
    Panel.background "#504945"
    Panel.children [
      drawPlayer resolution s.Player
    ]
  ]
