module XgbHmi.App.Program

open System
open Avalonia

[<CompiledName "BuildAvaloniaApp">]
let buildAvaloniaApp () =
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .WithInterFont()
        .LogToTrace()

[<EntryPoint; STAThread>]
let main argv =
    buildAvaloniaApp().StartWithClassicDesktopLifetime argv
