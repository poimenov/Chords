namespace SongBook.Avalonia

module Home =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    open Avalonia.FuncUI

    let view =
        Component.create (
            "Home",
            fun _ ->
                DockPanel.create
                    [ DockPanel.horizontalAlignment HorizontalAlignment.Center
                      DockPanel.verticalAlignment VerticalAlignment.Top
                      DockPanel.margin (0.0, 5.0, 0.0, 0.0)
                      DockPanel.children
                          [ TextBlock.create [ TextBlock.classes [ "title" ]; TextBlock.text "Главная страница" ] ] ]
        )
