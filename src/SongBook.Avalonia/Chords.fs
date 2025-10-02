namespace SongBook.Avalonia

open Chords

type QualityChordDiagrams =
    struct
        val Quality: ChordQuality
        val ChordDiagrams: ChordDiagram list

        new(quality, chordDiagrams) =
            { Quality = quality
              ChordDiagrams = chordDiagrams }
    end

module Chords =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    open Avalonia.FuncUI
    open Microsoft.FSharp.Reflection
    open System
    open Avalonia.Media
    open Avalonia.Svg

    let getChordQualityNames (cq: ChordQuality) =
        match cq with
        | Major -> "Мажорное трезвучие"
        | Minor -> "Минорное трезвучие /m/"
        | DominantSeventh -> "Доминантсептаккорд /7/"
        | Augmented -> "Увеличенное трезвучие /+5/"
        | DiminishedTriad -> "Уменьшенное трезвучие /m³/ (на самом деле dim или ° или m-5)"
        | MajorSixth -> "Мажорное трезвучие с секстой /6/"
        | MinorSixth -> "Минорное трезвучие с секстой /m6/"
        | MajorMajorSeventh -> "Большой мажорный септаккорд /maj7/"
        | DominantSeventhSharpFifth -> "Доминантсептаккорд с повышенной квинтой /7+5/"
        | DominantSeventhFlatFifth -> "Доминантсептаккорд с пониженной квинтой /7-5/"
        | MajorMinorSeventh -> "Большой минорный септаккорд /mMaj7/"
        | MinorSeventh -> "Малый минорный септаккорд /m7/"
        | AugmentedSeventh -> "Увеличенный септаккорд /+7/+5/"
        | HalfDiminished -> "Полууменьшенный септаккорд /m7b5/"
        | DiminishedSeventh -> "Уменьшенный септаккорд /dim7/"
        | MajorNinth -> "Большой нонаккорд /maj9/"
        | MinorNinth -> "Малый нонаккорд /m9/"

    let allChordQualities =
        FSharpType.GetUnionCases typeof<ChordQuality>
        |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> ChordQuality)

    let allBaseNotes =
        FSharpType.GetUnionCases typeof<BaseNote>
        |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> BaseNote)

    let builder = ChordBuilder("black", "white")

    let getChord (baseNote: BaseNote) (chordQuality: ChordQuality) =
        { BaseNote = baseNote
          Quality = chordQuality
          BassNote = None
          AdditionalInfo = "" }

    let getDiagrams (chord: Chord) = builder.GetChordDiagrams chord

    let getChordDiagrams (baseNote: BaseNote) =
        let chord = getChord baseNote

        allChordQualities
        |> Array.map (fun cq -> cq |> chord)
        |> Array.map (fun c -> QualityChordDiagrams(c.Quality, c |> getDiagrams))

    let getImage (item: ChordDiagram) =
        let svg = SvgImage()
        svg.Source <- SvgSource.LoadFromSvg(builder.Render item)

        Image.create [ Image.margin 4; Image.source svg ]

    let getImageColumn (index: int) =
        DataGridTemplateColumn.create
            [ DataGridTemplateColumn.cellTemplate (
                  DataTemplateView<_>.create (fun (data: QualityChordDiagrams) ->
                      getImage (data.ChordDiagrams |> List.item index))
              ) ]

    let getFirstColumn =
        DataGridTemplateColumn.create
            [ DataGridTextColumn.width (DataGridLength(1, DataGridLengthUnitType.Star))
              DataGridTemplateColumn.cellTemplate (
                  DataTemplateView<_>.create (fun (data: QualityChordDiagrams) ->
                      TextBlock.create
                          [ TextBlock.horizontalAlignment HorizontalAlignment.Stretch
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.margin 4
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.text (data.Quality |> getChordQualityNames) ])
              ) ]

    let view =
        Component.create (
            "Chords",
            fun ctx ->
                let selectedBaseNote = ctx.useState<BaseNote> BaseNote.A

                DockPanel.create
                    [ DockPanel.horizontalAlignment HorizontalAlignment.Center
                      DockPanel.verticalAlignment VerticalAlignment.Stretch
                      DockPanel.margin (0.0, 5.0, 0.0, 0.0)
                      DockPanel.children
                          [ StackPanel.create
                                [ DockPanel.dock Dock.Top
                                  StackPanel.children
                                      [ TextBlock.create
                                            [ TextBlock.classes [ "title" ]
                                              TextBlock.text $"Аккорды: {Convert.ToString selectedBaseNote.Current}" ]
                                        ComboBox.create
                                            [ ComboBox.margin 4
                                              ComboBox.width 100
                                              ComboBox.dataItems allBaseNotes
                                              ComboBox.selectedItem selectedBaseNote.Current
                                              ComboBox.onSelectedItemChanged (fun item ->
                                                  (match box item with
                                                   | :? BaseNote as c -> c
                                                   | _ -> failwith "Something went horribly wrong!")
                                                  |> selectedBaseNote.Set)
                                              ComboBox.itemTemplate (
                                                  DataTemplateView<_>.create (fun (data: BaseNote) ->
                                                      TextBlock.create
                                                          [ TextBlock.text (Convert.ToString data)
                                                            TextBlock.width 50
                                                            TextBlock.textAlignment TextAlignment.Right ])
                                              ) ]
                                        DataGrid.create
                                            [ DataGrid.isReadOnly true
                                              DataGrid.headersVisibility DataGridHeadersVisibility.None
                                              DataGrid.items (getChordDiagrams selectedBaseNote.Current)
                                              DataGrid.columns
                                                  [ getFirstColumn
                                                    getImageColumn 0
                                                    getImageColumn 1
                                                    getImageColumn 2
                                                    getImageColumn 3
                                                    getImageColumn 4 ] ]

                                        ] ] ] ]
        )
