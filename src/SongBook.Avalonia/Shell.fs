namespace SongBook.Avalonia

module Shell =
    open Avalonia
    open Avalonia.Input
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Hosts
    open Avalonia.FuncUI.Elmish
    open Avalonia.Layout
    open Avalonia.Media
    open FluentIcons.Avalonia
    open FluentIcons.Common
    open Microsoft.FSharp.Reflection
    open Microsoft.Extensions.DependencyInjection

    // --------- Model ---------
    type Page =
        | Home
        | Chords
        | About

    type Model = { IsPaneOpen: bool; CurrentPage: Page }

    let initModel =
        { IsPaneOpen = false
          CurrentPage = Home },
        Cmd.none

    // --------- Msg ---------
    type Msg =
        | TogglePane
        | Navigate of Page

    // --------- Update ---------
    let update msg model =
        match msg with
        | TogglePane ->
            { model with
                IsPaneOpen = not model.IsPaneOpen },
            Cmd.none
        | Navigate page ->
            { model with
                CurrentPage = page
                IsPaneOpen = false },
            Cmd.none

    // --------- View ---------
    let pages =
        FSharpType.GetUnionCases typeof<Page>
        |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> Page)

    let view (los: ILinkOpeningService) (model: Model) (dispatch: Msg -> unit) =
        DockPanel.create
            [ DockPanel.children
                  [ SplitView.create
                        [ SplitView.displayMode SplitViewDisplayMode.CompactInline
                          SplitView.isPaneOpen model.IsPaneOpen
                          SplitView.compactPaneLengthProperty 45
                          SplitView.background "#1e1e1e"
                          SplitView.paneBackground "#1e1e1e"
                          SplitView.openPaneLength 150.

                          SplitView.pane (
                              StackPanel.create
                                  [ StackPanel.orientation Orientation.Vertical
                                    StackPanel.horizontalAlignment HorizontalAlignment.Stretch
                                    StackPanel.verticalAlignment VerticalAlignment.Top
                                    StackPanel.spacing 5
                                    StackPanel.children
                                        [ Button.create
                                              [ Button.horizontalAlignment HorizontalAlignment.Left
                                                Button.margin 5
                                                Button.content (
                                                    SymbolIcon.create
                                                        [ SymbolIcon.width 24
                                                          SymbolIcon.height 24
                                                          SymbolIcon.symbol Symbol.TextAlignJustify ]

                                                )
                                                Button.onClick (fun _ -> dispatch TogglePane) ]
                                          ListBox.create
                                              [ ListBox.padding 0
                                                ListBox.margin (2, 0, -100, 0)
                                                ListBox.classes [ "menu" ]
                                                ListBox.dataItems pages
                                                ListBox.onSelectedItemChanged (fun item ->
                                                    match box item with
                                                    | null -> ()
                                                    | :? Page as p -> dispatch (Navigate p)
                                                    | _ -> failwith "Something went horribly wrong!")
                                                ListBox.itemTemplate (
                                                    DataTemplateView<_>.create (fun (data: Page) ->
                                                        let label, symbol =
                                                            match data with
                                                            | Home -> "Home", Symbol.Home
                                                            | Chords -> "Chords", Symbol.MusicNote1
                                                            | About -> "About", Symbol.Info

                                                        StackPanel.create
                                                            [ StackPanel.orientation Orientation.Horizontal
                                                              StackPanel.spacing 17
                                                              StackPanel.children
                                                                  [ SymbolIcon.create
                                                                        [ SymbolIcon.symbol symbol
                                                                          SymbolIcon.background "transparent"
                                                                          SymbolIcon.width 24
                                                                          SymbolIcon.height 24
                                                                          SymbolIcon.margin (2, 0, 2, 0) ]
                                                                    TextBlock.create
                                                                        [ TextBlock.text label
                                                                          TextBlock.textTrimming
                                                                              TextTrimming.CharacterEllipsis
                                                                          TextBlock.textWrapping TextWrapping.NoWrap
                                                                          TextBlock.fontSize 14.0 ] ] ])
                                                ) ] ] ]
                          )

                          SplitView.content (
                              DockPanel.create
                                  [ DockPanel.children
                                        [ Border.create
                                              [ Border.cornerRadius (12, 0, 0, 0)
                                                Border.background "#2d2d2d"
                                                Border.child (
                                                    match model.CurrentPage with
                                                    | Home -> Home.view
                                                    | Chords -> Chords.view
                                                    | About -> About.view los
                                                ) ] ]

                                    ]
                          ) ] ] ]

    // --------- App ---------
    type MainWindow(serviceProvider: ServiceProvider) as this =
        inherit HostWindow()
        let los = serviceProvider.GetService<ILinkOpeningService>()

        do
            base.Title <- "Full App"
            base.Width <- 800.0
            base.Height <- 600.0
            base.MinWidth <- 800.0
            base.MinHeight <- 600.0
#if DEBUG
            this.AttachDevTools(KeyGesture Key.F12)
#endif

            let w = view los

            Elmish.Program.mkProgram (fun () -> initModel) update w
            |> Program.withHost this
            |> Program.run
