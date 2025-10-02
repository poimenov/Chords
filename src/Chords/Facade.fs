[<AutoOpen>]
module Chords.Facade

type ChordBuilder(backGroundColor: string, foreGroundColor: string) =
    member this.Render(chord: ChordDiagram) =
        renderChordDiagram (chord, backGroundColor, foreGroundColor)
    member this.Render(chordName: string) =
        let chord = parseChord chordName

        match chord with
        | None -> ""
        | Some c ->
            let diagram = generateChordDiagram c
            renderChordDiagram (diagram, backGroundColor, foreGroundColor)

    member this.Render(chord: Chord) =
        renderChordDiagram (generateChordDiagram chord, backGroundColor, foreGroundColor)        

    member this.CreateFret(fret: int, finger: int) = createFret fret finger

    member this.GetChordDiagrams(chordName: string) =
        let chord = parseChord chordName

        match chord with
        | None -> []
        | Some c -> generateChordDiagrams c

    member this.GetChordDiagrams(chord: Chord) = generateChordDiagrams chord
