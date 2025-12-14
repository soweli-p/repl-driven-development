module Cubicle.View

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open Avalonia.Controls
open Elmish

open Cubicle.Update
open Avalonia
open Avalonia.Media.Imaging
open Avalonia.Media


let hitboxToPoints resolution offset (h: Hitbox) = 
  let hB = {
    Min = resolution - h.Min  + offset
    Max = resolution - h.Max + offset
  }
  let minX = h.Min.X - offset.X
  let maxX = h.Max.X - offset.X
  [
    new Point(minX, min resolution.Y hB.Min.Y)
    new Point(minX, min resolution.Y hB.Max.Y)
    new Point(maxX, min resolution.Y hB.Max.Y)
    new Point(maxX, min resolution.Y hB.Min.Y)
  ]

let getOffset (resolution: Vector) (p: Player) =
  let halfRes = resolution * 0.5
  let yRes = resolution * 0.2
  let pPos = p.Position
  let xOffset = if pPos.X >= halfRes.X then pPos.X - halfRes.X else pPos.X - halfRes.X
  let yOffset = if pPos.Y >= halfRes.Y then pPos.Y - yRes.Y else pPos.Y - yRes.Y
  new Vector(xOffset, yOffset)

let drawPlatforms res offset = 
  List.map (fun platform ->
    Polygon.create [
      let points = hitboxToPoints res offset platform.Hitbox
      Shapes.Polygon.points points
      Shapes.Polygon.fill "#83a598"
    ] :> IView)

let drawPlayer res offs (p: Player): IView = 
  Polygon.create [
    let points = hitboxToPoints res offs (getHitbox p)
    Shapes.Polygon.points points
    Shapes.Polygon.fill "#fe8019"
  ]
let viewFn resolution (s: GameState) (d: Dispatch<Event>): IView = 
  let offset = getOffset resolution s.Player
  Panel.create [
    Panel.background "#504945"
    Panel.children <|
      drawPlayer resolution offset s.Player
      :: drawPlatforms resolution offset s.Platforms
  ]
