namespace SongBook.Avalonia

module About =
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Types
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.FuncUI.DSL

    type Links =
        | AvaloniaRepository
        | AvaloniaAwesome
        | FuncUIRepository
        | FuncUISamples

    let openUrl url (los: ILinkOpeningService) =
        let url =
            match url with
            | AvaloniaRepository -> "https://github.com/AvaloniaUI/Avalonia"
            | AvaloniaAwesome -> "https://github.com/AvaloniaCommunity/awesome-avalonia"
            | FuncUIRepository -> "https://github.com/fsprojects/Avalonia.FuncUI"
            | FuncUISamples -> "https://github.com/fsprojects/Avalonia.FuncUI/tree/master/src/Examples"

        los.OpenUrl url

    let headerView (dock: Dock) : IView =
        StackPanel.create
            [ StackPanel.dock dock
              StackPanel.verticalAlignment VerticalAlignment.Top
              StackPanel.horizontalAlignment HorizontalAlignment.Stretch
              StackPanel.children
                  [ TextBlock.create
                        [ TextBlock.classes [ "title" ]
                          TextBlock.text "Thank you for using Avalonia.FuncUI" ]
                    TextBlock.create
                        [ TextBlock.classes [ "subtitle" ]
                          TextBlock.text (
                              "Avalonia.FuncUI is a project that provides you with an Elmish DSL for Avalonia Controls"
                              + "for you to use in an F# idiomatic way. We hope you like the project and spread the word :)\n"
                              + "Questions ? Reach to us on Gitter, also check the links below"
                          ) ] ] ]


    let avaloniaLinksView (dock: Dock) (los: ILinkOpeningService) : IView =
        StackPanel.create
            [ StackPanel.dock dock
              StackPanel.horizontalAlignment HorizontalAlignment.Left
              StackPanel.children
                  [ TextBlock.create [ TextBlock.classes [ "title" ]; TextBlock.text "Avalonia" ]
                    TextBlock.create
                        [ TextBlock.classes [ "link" ]
                          TextBlock.onTapped (fun _ -> openUrl AvaloniaRepository los)
                          TextBlock.text "Avalonia Repository" ]
                    TextBlock.create
                        [ TextBlock.classes [ "link" ]
                          TextBlock.onTapped (fun _ -> openUrl AvaloniaAwesome los)
                          TextBlock.text "Awesome Avalonia" ] ] ]

    let avaloniaFuncUILinksView (dock: Dock) (los: ILinkOpeningService) : IView =
        StackPanel.create
            [ StackPanel.dock dock
              StackPanel.horizontalAlignment HorizontalAlignment.Right
              StackPanel.children
                  [ TextBlock.create [ TextBlock.classes [ "title" ]; TextBlock.text "Avalonia.FuncUI" ]
                    TextBlock.create
                        [ TextBlock.classes [ "link" ]
                          TextBlock.onTapped (fun _ -> openUrl FuncUIRepository los)
                          TextBlock.text "Avalonia.FuncUI Repository" ]
                    TextBlock.create
                        [ TextBlock.classes [ "link" ]
                          TextBlock.onTapped (fun _ -> openUrl FuncUISamples los)
                          TextBlock.text "Samples" ] ] ]

    let view (los: ILinkOpeningService) =
        Component.create (
            "About",
            fun _ ->
                DockPanel.create
                    [ DockPanel.horizontalAlignment HorizontalAlignment.Stretch
                      DockPanel.verticalAlignment VerticalAlignment.Top
                      DockPanel.margin (10.0, 5.0, 10.0, 0.0)
                      DockPanel.children
                          [ headerView Dock.Top
                            avaloniaLinksView Dock.Left los
                            avaloniaFuncUILinksView Dock.Right los ] ]
        )
