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

module Platforms =
  type PlatformType = ShortPlatform | LongPlatform | SideWall | Floor
  type Platform = {
    Position: Vector
    Type: PlatformType
  } with 
    member this.GetHitbox =
      match this.Type with 
      | ShortPlatform -> {Min = this.Position; Max = this.Position + new Vector(512, 64)}
      | LongPlatform -> {Min = this.Position; Max = this.Position + new Vector(256, 32)}
      | Floor -> {Min = this.Position; Max = this.Position + new Vector(2048, 512)}    
      | SideWall -> {Min = this.Position; Max = this.Position + new Vector(32, 4096)}


  let mk t x y = {Position = new Vector(x, y); Type = t}

module Clouds = 
  type CloudType = Cloud1 | Cloud2 | Long | Smol | Big
    with static member cases = [|Cloud1; Cloud2; Long; Smol; Big |]
  type Cloud = {
    Position: Vector
    Type: CloudType
    Velocity: Vector
  }

  open System
  let random = new Random()
  type CloudVelocity = Slow | Fast | VerySlow 
    with static member cases = [|Slow; Fast; VerySlow|]

  let generateRandomCloud pos = 
    let cloudType = random.Next() % 5 |> fun i -> CloudType.cases[i]
    let cloudVelocity = 
      match random.Next() % 3 |>  fun i -> CloudVelocity.cases[i] with
      | Slow -> new Vector(0.1, 0.0) * 1.2
      | Fast -> new Vector(0.5, 0.0) * 1.2
      | VerySlow -> new Vector(0.05, 0.0) * 1.2
    let velocityScale = 
      match cloudType with 
      | Big -> 0.3
      | _ -> 1.0
    {Position = pos; Type = cloudType; Velocity = cloudVelocity * velocityScale}


type GameState = {
  Player: Player
  Platforms: Platforms.Platform list
  Clouds: Clouds.Cloud list
  TickCounter: int
}

module Lenses = 

  [<AutoOpen>]
  module Player =
      let inline _pos f (p: Player) =
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
      let inline _clouds f p =
          f p.Clouds <&> fun x -> { p with Clouds = x }
      let inline _counter f p =
          f p.TickCounter <&> fun x -> { p with TickCounter = x }

  module Clouds =
      let inline _vel f (c: Clouds.Cloud) =
          f c.Velocity <&> fun x -> { c with Velocity = x }
      let inline _pos f (c: Clouds.Cloud) =
          f c.Position <&> fun x -> { c with Position = x }

  let inline _1v f (v: Vector) = f v.X <&> fun x -> new Vector(x, v.Y)
  let inline _2v f (v: Vector) = f v.Y <&> fun y -> new Vector(v.X, y)

  let (|Vec2|) (v: Vector) = struct(v.X, v.Y)

  let velL = _player << _vel
  let posL = _player << _pos
  let dragL = _player << _drag

type Event = 
  | Tick
  | KeyPressed of Key
  | KeyReleased of Key



module InitInternals = 
  open Platforms
  let floor = mk Floor -512 -512
  let platformWidth = 32.0
  
  module CloudsGeneration = 
    open Clouds
    let cloudZone = 
      let floor = getHitbox floor
      //{Min = new Vector(floor.Min.X, 256); Max = new Vector(512, 1024.0)}
      //{Min = new Vector(floor.Min.X - 512.0, 256); Max = new Vector(floor.Max.X, 1024.0)}
      {Min = new Vector(-1024, 256); Max = new Vector(2048, 1024.0)}
    let random = new System.Random()
    let generateCloud (existingClouds: Cloud list) = 
      let size = cloudZone.Max - cloudZone.Min
      let getPos () = 
        let x = (random.Next() % int size.X) + (int cloudZone.Min.X * 2)
        let y = (random.Next() % int size.Y) + (int cloudZone.Min.Y)
        new Vector(float x, float y)
      let mutable pos = getPos ()
      while existingClouds |> List.exists (fun cl -> Vector.Distance(pos, cl.Position) <= 128) do
        pos <- getPos ()
      generateRandomCloud pos
    let cloudCount = 10
    let rec generateClouds existing = function
      | 0 -> existing
      | n -> generateClouds (generateCloud existing :: existing) (n - 1)

open InitInternals

let getPlatforms () = [
    floor
    Platforms.mk Platforms.ShortPlatform 100 200
    Platforms.mk Platforms.ShortPlatform 400 400 
    Platforms.mk Platforms.ShortPlatform 1000 400 
    Platforms.mk Platforms.ShortPlatform 1400 800
  ]
let initFn (): GameState = 
  { Player = { Position = new Vector(100, 300)
               Grounded = false
               Drag = 0.0
               Velocity = new Vector(0, 0)}
    Clouds = CloudsGeneration.generateClouds [] 40
    TickCounter = 0
    Platforms = getPlatforms () }
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
          let pVel, pPos = player.Velocity, player.Position
          let {Min = playerMin; Max = playerMax} = getHitbox player
          let {Min = platMin; Max = platMax} = getHitbox platform
          let isFalling = pVel.Y <= 0.0
          if  isFalling && playerMin.Y >= platMin.Y && playerMax.Y >= platMax.Y then //when player is on topside
              let newPos = pPos |> _2v .-> platMax.Y
              {player with Position = newPos; Grounded = true}
          else if playerMax.Y >= platMin.Y && playerMin.Y < platMin.Y then // from down
              { player with Position = setl _2v (platMin.Y - playerSize.Y) pPos
                            Velocity = setl _2v (-g) pVel }
          else if playerMin.X <= platMin.X && playerMax.X <= platMax.X then // from left
              { player with Position = setl _1v (platMin.X - playerSize.X) pPos }
          else 
              { player with Position = setl _1v platMax.X pPos } // from right
                
        over _player (collidedPlatforms |> flip (List.fold foldFn)) s

module CloudsGeneration = 
  open InitInternals.CloudsGeneration
  let applyCloudsVel = over _clouds (List.map (fun c -> over Clouds._pos ((+) c.Velocity) c))
  let moveBackClouds = over _clouds (List.map (fun c -> 
    if c.Position.X > cloudZone.Max.X then 
      {c with Position = setl _1v cloudZone.Min.X c.Position} 
    else c))

open Physics
open Gravity
open Drag
open Collision

let tickCounter = over _counter ((+) 1)

let liveEditPlatforms s = {s with Platforms = getPlatforms ()}

let updateFn = function 
  | Tick -> applyVel >> applyDrag >> applyG >> checkCollision >> CloudsGeneration.applyCloudsVel >> CloudsGeneration.moveBackClouds >> tickCounter >> liveEditPlatforms
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
