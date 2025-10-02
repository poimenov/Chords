namespace SongBook.Avalonia

open System
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI
open Avalonia.Themes.Fluent

type App() =
    inherit Application()
    let serviceProvider = configureServices ()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.Styles.Load "avares://Avalonia.Controls.DataGrid/Themes/Fluent.xaml"
        this.Styles.Load "avares://SongBook.Avalonia/Styles.xaml"
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            AppDomain.CurrentDomain.UnhandledException.Add(fun e ->
                let ex = e.ExceptionObject :?> Exception
                System.Diagnostics.Debug.WriteLine ex)

            desktopLifetime.MainWindow <- Shell.MainWindow serviceProvider
        | _ -> ()

module Program =
    [<STAThread>]
    [<EntryPoint>]
    let main (args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime
            args
