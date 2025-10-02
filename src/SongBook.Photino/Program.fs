module Program

open Photino.Blazor
open Microsoft.Extensions.DependencyInjection
open Microsoft.FluentUI.AspNetCore.Components
open SongBook.Photino
open System

[<STAThread>]
[<EntryPoint>]
let main args =
    let builder = PhotinoBlazorAppBuilder.CreateDefault(args)

    builder.RootComponents.Add<App> "#app"
    builder.Services.AddFunBlazorWasm() |> ignore
    builder.Services.AddFluentUIComponents() |> ignore
    builder.Services.AddSingleton<IPlatformService, PlatformService>() |> ignore
    builder.Services.AddSingleton<IProcessService, ProcessService>() |> ignore

    builder.Services.AddSingleton<ILinkOpeningService, LinkOpeningService>()
    |> ignore

    let application = builder.Build()

    // customize window
    application.MainWindow
        .SetSize(1110, 768)
        .SetIconFile("wwwroot/favicon.ico")
        .SetTitle
        "SongBook"
    |> ignore

    AppDomain.CurrentDomain.UnhandledException.Add(fun e ->
        let ex = e.ExceptionObject :?> Exception
        application.MainWindow.ShowMessage(ex.Message, "Error") |> ignore)

    application.Run()
    0
