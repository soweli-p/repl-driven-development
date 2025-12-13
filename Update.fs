module Cubicle.Update
open Avalonia.Input
open Avalonia
open FSharpPlus.Lens


let playerSize = new Vector(64, 64)
type Player = {
  Position: Vector
  Velocity: Vector
}
type GameState = {
  Player: Player
}

module Lenses = 

  [<AutoOpen>]
  module Player =
      let inline _pos f p =
          f p.Position <&> fun x -> { p with Position = x }
      let inline _vel f p =
          f p.Velocity <&> fun x -> { p with Velocity = x }

  [<AutoOpen>]
  module State =
      let inline _player f p =
          f p.Player <&> fun x -> { p with Player = x }

  let inline _1v f (v: Vector) = f v.X <&> fun x -> new Vector(x, v.Y)
  let inline _2v f (v: Vector) = f v.Y <&> fun y -> new Vector(v.X, y)

  let velL = _player << _vel
  let posL = _player << _pos

type Event = 
  | Tick
  | KeyPressed of Key
  | KeyReleased of Key


let initFn (): GameState = 
  { Player = { Position = new Vector(100, 300)
               Velocity = new Vector(0, 0)} }

open Lenses

module Physics = 
  let playerVeocity = 10.0
  let applyVel s = over posL ((+) s.Player.Velocity) s

open Physics
let updateFn = function 
  | Tick -> applyVel
  | KeyPressed Key.W -> over (velL << _2v) ((+) playerVeocity)
  | KeyPressed Key.A -> over (velL << _1v) ((+) -playerVeocity)
  | KeyPressed Key.S -> over (velL << _2v) ((+) -playerVeocity)
  | KeyPressed Key.D -> over (velL << _1v) ((+) playerVeocity)
  | KeyReleased Key.W -> setl (velL << _2v) 0.0
  | KeyReleased Key.A -> setl (velL << _1v) 0.0 
  | KeyReleased Key.S -> setl (velL << _2v) 0.0
  | KeyReleased Key.D -> setl (velL << _1v) 0.0
  | _ -> id
  //| _ -> id
