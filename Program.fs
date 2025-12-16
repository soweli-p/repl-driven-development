module Cubicle.Program

open System
open Avalonia
open Avalonia.Themes.Fluent
open Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Controls.ApplicationLifetimes

open FSharpPlus
open Avalonia.Threading

open Cubicle.Update

type MainWindow() as this =
    inherit HostWindow()
    do
        let subscriptions (_state: GameState) =
          let timerSub (dispatch: Dispatch<Event>) =
            let invoke () =
                Event.Tick |> dispatch
                true

            DispatcherTimer.Run(invoke, TimeSpan.FromMilliseconds 16.0)

          let keyDownSub (dispatch: Dispatch<Event>) =
              this.KeyDown.Subscribe(fun eventArgs -> KeyPressed eventArgs.Key |> dispatch)
          let keyUpSub (dispatch: Dispatch<Event>) =
              this.KeyUp.Subscribe(fun eventArgs -> KeyReleased eventArgs.Key |> dispatch)

          [ [ nameof timerSub ], timerSub
            [ nameof keyDownSub ], keyDownSub 
            [ nameof keyUpSub ], keyUpSub 
          ]
        base.Title <- "Cubicle"

        let res = new Vector(1920, 800)
        let getRes () = new Vector(this.Width, this.Height)
        base.Width <- res.X
        base.Height <- res.Y

        Program.mkSimple Update.initFn Update.updateFn (View.viewFn getRes)
        |> Program.withHost this
        |> Program.withSubscription subscriptions
        |> Program.run


type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let mainWindow = MainWindow()
            desktopLifetime.MainWindow <- mainWindow
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)

