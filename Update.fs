module Cubicle.Update
open Avalonia.Input
open Avalonia
open FSharpPlus.Lens
open FSharpPlus


type Hitbox = {
  Min: Vector
  Max: Vector
}
let inline getHitbox<'a when 'a:(member GetHitbox: Hitbox)> (x: 'a) = x.GetHitbox

let playerSize = new Vector(64, 64)
type Player = {
  Position: Vector
  Velocity: Vector
  Grounded: bool
  Drag: float
} with member this.GetHitbox = {Min = this.Position; Max = this.Position + playerSize}

type Platfotm = {
  Hitbox: Hitbox
} with member this.GetHitbox = this.Hitbox

type GameState = {
  Player: Player
  Platforms: Platfotm list
}

module Lenses = 

  [<AutoOpen>]
  module Player =
      let inline _pos f p =
          f p.Position <&> fun x -> { p with Position = x }
      let inline _vel f p =
          f p.Velocity <&> fun x -> { p with Velocity = x }
      let inline _drag f p =
          f p.Drag <&> fun x -> { p with Drag = x }
      let inline _grounded f p =
          f p.Grounded <&> fun x -> { p with Grounded = x }

  [<AutoOpen>]
  module State =
      let inline _player f p =
          f p.Player <&> fun x -> { p with Player = x }

  let inline _1v f (v: Vector) = f v.X <&> fun x -> new Vector(x, v.Y)
  let inline _2v f (v: Vector) = f v.Y <&> fun y -> new Vector(v.X, y)

  let velL = _player << _vel
  let posL = _player << _pos
  let dragL = _player << _drag

type Event = 
  | Tick
  | KeyPressed of Key
  | KeyReleased of Key



module InitInternals = 
  let floor = { Hitbox = {Min = Vector(0, 0); Max = Vector (20000, 40)} }
  let platformWidth = 30.0
  let mkPlatform pos length = {Hitbox = { Min = pos; Max = new Vector(pos.X + length, pos.Y + platformWidth)}}

open InitInternals
let initFn (): GameState = 
  { Player = { Position = new Vector(100, 300)
               Grounded = false
               Drag = 0.0
               Velocity = new Vector(0, 0)}
    Platforms = floor :: 
                [ mkPlatform (new Vector(100, 200)) 200
                  mkPlatform (new Vector(400, 400)) 200] }

open Lenses

module Physics = 
  let playerVelocity = 10.0

  module Gravity = 
    let g = 10.0
    let applyG = function
      | s when s.Player.Velocity.Y > -g && not s.Player.Grounded ->
        over (velL << _2v) (fun v -> v - g / 10.0) s
      | s -> setl (velL << _2v) (-g) s

    let jump = setl (velL << _2v) (2.0*g) >> setl (_player << _grounded) false
  module Drag = 
    let dragForce = 2.0

    let applyDrag (s: GameState) =
      match s.Player.Drag, s.Player.Velocity.X with
      | d, v when d > 0 && v > 0 || d < 0 && v < 0
          -> over (velL << _1v) ((+) -d) s
      | d, _  when d <> 0 -> 
        setl dragL 0 s
      | _, _ -> s

  let playerVeocity = 10.0
  let applyVel s = over posL ((+) s.Player.Velocity) s

  module Collision = 
    open Gravity
    let inline collides h1 h2 =
        let a = getHitbox h1
        let b = getHitbox h2
        a.Max.X > b.Min.X &&
        a.Min.X < b.Max.X &&
        a.Max.Y > b.Min.Y &&
        a.Min.Y < b.Max.Y

    let checkCollision s = 
        let collidedPlatforms = s.Platforms |> List.filter (collides s.Player)
        let foldFn player platform = 
          let pPos, pVel = player.Position, player.Velocity
          let {Min = playerMin; Max = playerMax} = player.GetHitbox
          let {Min = platMin; Max = platMax} = platform.Hitbox
          let isFalling = pVel.Y <= 0.0
          if  isFalling && playerMin.Y >= platMin.Y && playerMax.Y >= platMax.Y then //when player is on topside
              let newPos = pPos |> _2v .-> platform.Hitbox.Max.Y
              {player with Position = newPos; Grounded = true}
          else if playerMax.Y >= platMin.Y && playerMin.Y < platMin.Y then // from down
              { player with Position = setl _2v (platMin.Y - playerSize.Y) pPos
                            Velocity = setl _2v (-g) pVel }
          else if playerMin.X <= platMin.X && playerMax.X <= platMax.X then // from left
              { player with Position = setl _1v (platMin.X - playerSize.X) pPos }
          else 
              { player with Position = setl _1v platMax.X pPos } // from right
                
        over _player (collidedPlatforms |> flip (List.fold foldFn)) s

open Physics
open Gravity
open Drag
open Collision

let updateFn = function 
  | Tick -> applyVel >> applyDrag >> applyG >> checkCollision
  | KeyPressed Key.Space -> function 
      | s when s.Player.Grounded -> jump s 
      | s -> s
  | KeyPressed Key.A -> setl (velL << _1v) -playerVelocity >> setl dragL 0
  | KeyPressed Key.D -> setl (velL << _1v) playerVelocity >> setl dragL 0
  | KeyPressed Key.S -> over (velL << _2v) ((+) -playerVeocity)
  | KeyReleased Key.A -> setl dragL -dragForce
  | KeyReleased Key.D -> setl dragL dragForce
  | KeyReleased Key.S -> over (velL << _2v) ((+) (-2.0*g))
  | _ -> id
