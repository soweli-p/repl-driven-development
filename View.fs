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
    new Point(minX, hB.Min.Y)
    new Point(minX, hB.Max.Y)
    new Point(maxX, hB.Max.Y)
    new Point(maxX, hB.Min.Y)
  ]

let getOffset (resolution: Vector) (p: Player) =
  let halfRes = resolution * 0.5
  let yRes = resolution * 0.2
  let pPos = p.Position
  let xOffset = if pPos.X >= halfRes.X then pPos.X - halfRes.X else pPos.X - halfRes.X
  let yOffset = if pPos.Y >= halfRes.Y then pPos.Y - yRes.Y else pPos.Y - yRes.Y
  new Vector(xOffset, yOffset)

let mkBrush bitMap x y width height = 
  let brush = bitMap |> ImageBrush
  brush.SourceRect <- new RelativeRect(float x, float y, float width, float height, RelativeUnit.Absolute)
  brush

let mkPolygon resolution offset hitbox (brush: IBrush) = Polygon.create [
  let points = hitboxToPoints resolution offset hitbox
  Control.bitmapInterpolationMode BitmapInterpolationMode.None
  Shapes.Polygon.points points
  Shapes.Polygon.fill brush
]

module Player =
  let playerMap = new Bitmap ("Assets/player.png")
  let playerDefault = mkBrush playerMap 0 0 16 16
  let playerRight = mkBrush playerMap 16 0 16 16

  let playerLeft = 
    let playerLeft = mkBrush playerMap 16 0 16 16
    let mirrorMatrix = 
      Matrix.CreateScale (new Vector(-1.0, 1.0))
      |> _.Append
      <| Matrix.CreateTranslation(new Vector(playerSize.X, 0.0))
    playerLeft.Transform <- new MatrixTransform(mirrorMatrix)
    playerLeft

  let playerDown = mkBrush playerMap 0 16 16 16
  let playerJump = mkBrush playerMap 16 16 16 16

  module Glow = 
    let playerGlow = new Bitmap ("Assets/playerGlow.png")
    let mkBrush x y h w =
      let br = mkBrush playerGlow x y h w
      br.Opacity <- 0.3
      br
    let glow1 = mkBrush 0 0 16 16
    let glow2 = mkBrush 16 0 16 16
    let glow3 = mkBrush 0 16 16 16
    let glow4 = mkBrush 16 16 16 16
    let getGlow tick = 
      match tick % 60.0 * 0.5 with
      | x when x < 15.0 * 0.5-> glow1
      | x when x < 30.0 * 0.5 -> glow2
      | x when x < 45.0 * 0.5 -> glow3
      | _ -> glow4

  open Cubicle.Update.Lenses

  let drawPlayer res offs tick (p: Player): IView = 
    let glow = Glow.getGlow tick
    glow.Opacity <- 0.3
    let playerHitbox = getHitbox p
    let glowHitbox = 
      let p = playerHitbox
      {Min = new Vector(p.Min.X - 4.0, p.Min.Y - 6.0); Max = new Vector(p.Max.X + 4.0, p.Max.Y + 4.0)}
    let tile =
      match p.Velocity with
      | Vec2(_, y) when y > 0 -> playerJump
      | Vec2(_, y) when y < 0 && not p.Grounded -> playerDown
      | Vec2(x, _) when x < 0 -> playerLeft
      | Vec2(x, _) when x > 0 -> playerRight
      | _ -> playerDefault
    Panel.create [
      Panel.children [ 
        mkPolygon res offs playerHitbox tile
        mkPolygon res offs glowHitbox glow 
      ]
    ]

module Platforms = 
  (*
  let wallBrush = new Bitmap("Assets/wall.png") |> ImageBrush
      *)
  //let floorBrush = mkBrush (new Bitmap "Assets/floorPlatform.png") 0 0 2048 1024
  let floorBrush = new Bitmap "Assets/floorPlatform.png" |> ImageBrush
   
  let shortPlatformBrush = 
    new Bitmap "Assets/shortPlatform.png" |> ImageBrush
    //new Bitmap("Assets/shortPlatform.png") |> ImageBrush
  //let longPlatformBrush = new Bitmap("Assets/LongPlatform.png") |> ImageBrush

  open Platforms
  let drawPlatforms resolution offs = List.map (fun platform -> 
    let hb = getHitbox platform
    let points = hitboxToPoints resolution offs hb
    floorBrush.Stretch <- Stretch.UniformToFill
    shortPlatformBrush.Stretch <- Stretch.UniformToFill
    let brush =
      match platform.Type with 
      | ShortPlatform -> Shapes.Polygon.fill shortPlatformBrush
      | Floor -> Shapes.Polygon.fill floorBrush
      | _ -> Shapes.Polygon.fill "#292929"
    Polygon.create [
      Control.bitmapInterpolationMode BitmapInterpolationMode.None
      Shapes.Polygon.points points
      brush
    ] :> IView
      )
module Background =
  let color = "#0a2857"
  let colorBrush =
    let b = new LinearGradientBrush()
    b.StartPoint <- RelativePoint.Parse "50%,0%"
    b.EndPoint <- RelativePoint.Parse "50%,100%"
    let stops = new GradientStops()
    let stop1 = new GradientStop()
    stop1.Color <- Color.Parse "#070a12"
    stop1.Offset <- 0.0
    let stop2 = new GradientStop()
    stop2.Color <- Color.Parse "#17346d"
    stop2.Offset <- 1.0
    stops.AddRange [stop1; stop2]
    b.GradientStops <- stops
    b
  let moonBrush = new Bitmap "Assets/moon.png" |> ImageBrush

  module Clouds = 
    open Update.Clouds
    let cloudsMap = new Bitmap "Assets/clouds.png"
    let cloudScale = 4.0

    let mkBrush bitmap x y w h = 
      let brush = mkBrush bitmap x y w h
      brush.Opacity <- 0.9
      brush
    let cloud1 = mkBrush cloudsMap 0 0 64 32
    let cloud2 = mkBrush cloudsMap 64 0 64 32
    let bigCloud1 = mkBrush cloudsMap 0 32 96 32
    let smolCloud = mkBrush cloudsMap 96 32 32 32
    let bigCloud2 = mkBrush cloudsMap 0 64 128 64

    let drawCloud res (offs: Vector) (cloud: Cloud) = 
      let brush = 
        match cloud.Type with
        | Cloud1 -> cloud1
        | Cloud2 -> cloud2
        | Long -> bigCloud1
        | Smol -> smolCloud
        | Big -> bigCloud2
      let parallax = 
        match cloud.Type with 
        | Big -> 0.05
        | _ -> 0.1
      let size = new Vector(brush.SourceRect.Rect.Width, brush.SourceRect.Rect.Height) * cloudScale
      let hb = {Min = cloud.Position; Max = cloud.Position + size}
      mkPolygon res (offs * parallax) hb brush :> IView
    
    let drawClouds res (offs: Vector) clouds =
      let poygons = 
        clouds
        |> List.sortBy (fun c -> match c.Type with | Big -> 1 | _ -> 2)
        |> List.map (drawCloud res offs)
      Panel.create [ Panel.children poygons ] :> IView

  let drawMoon res (offs: Vector) = 
    let moonSize = new Vector(42, 43) * 4.0
    let moonPos = new Vector(1000, 500)
    let hb = {Min = moonPos; Max = moonPos + moonSize}
    let offs = offs * 0.02
    let points = hitboxToPoints res offs hb
    Polygon.create [
      Control.bitmapInterpolationMode BitmapInterpolationMode.None
      Shapes.Polygon.points points
      Shapes.Polygon.fill moonBrush
    ]


let viewFn getRes (s: GameState) (d: Dispatch<Event>): IView = 
  let resolution = getRes ()
  let offset = getOffset resolution s.Player
  Panel.create [
    Panel.background Background.colorBrush
    Panel.children <|
      Background.drawMoon resolution offset
      :: Background.Clouds.drawClouds resolution offset s.Clouds
      :: Player.drawPlayer resolution offset s.TickCounter s.Player
      :: Platforms.drawPlatforms resolution offset s.Platforms
  ]
