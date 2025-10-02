[<AutoOpen>]
module SongBook.Web.Chords

open Microsoft.FSharp.Reflection
open Chords
open Giraffe.ViewEngine

let getChordQualityNames (cq: ChordQuality) =
    match cq with
    | Major -> "Major triad"
    | Minor -> "Minor triad /m/"
    | DominantSeventh -> "Dominant seventh chord /7/"
    | Augmented -> "Augmented triad /+5/"
    | DiminishedTriad -> "Diminished triad /m³/ (actually dim or ° or m-5)"
    | MajorSixth -> "Major triad with a sixth /6/"
    | MinorSixth -> "Minor triad with a sixth /m6/"
    | MajorMajorSeventh -> "Major seventh chord /maj7/"
    | DominantSeventhSharpFifth -> "Dominant seventh chord with a raised fifth /7+5/"
    | DominantSeventhFlatFifth -> "Dominant seventh chord with a flatted fifth /7-5/"
    | MajorMinorSeventh -> "Major minor seventh chord /mMaj7/"
    | MinorSeventh -> "Minor minor seventh chord /m7/"
    | AugmentedSeventh -> "Augmented seventh chord /+7/+5/"
    | HalfDiminished -> "Half-diminished seventh chord /m7b5/"
    | DiminishedSeventh -> "Diminished seventh chord /dim7/"
    | MajorNinth -> "Major ninth chord /maj9/"
    | MinorNinth -> "Minor ninth chord /m9/"

let allChordQualities =
    FSharpType.GetUnionCases typeof<ChordQuality>
    |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> ChordQuality)

let allBaseNotes =
    FSharpType.GetUnionCases typeof<BaseNote>
    |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> BaseNote)

let builder = ChordBuilder("white", "black")

let getDiagrams (chord: Chord) = builder.GetChordDiagrams chord

let getChordsSelect (baseNote: BaseNote) =

    let getValue (bn: BaseNote) =
        (string bn)
            .Replace("sharp", "♯", System.StringComparison.OrdinalIgnoreCase)
            .Replace("flat", "♭", System.StringComparison.OrdinalIgnoreCase)

    div
        []
        [ str "Select the base note:"
          select
              [ _class "baseNotes"; _id "baseNotes" ]
              [ for bn in allBaseNotes do
                    option
                        [ _value (getValue bn)
                          if bn = baseNote then
                              _selected ]
                        [ str (getValue bn) ] ] ]

let getChordsTable (baseNote: BaseNote) =
    table
        []
        [ for q in allChordQualities do
              tr
                  []
                  [ td [ _class "quality-name" ] [ str (getChordQualityNames q) ]
                    let chord =
                        { BaseNote = baseNote
                          Quality = q
                          BassNote = None
                          AdditionalInfo = "" }

                    for d in getDiagrams chord do
                        td [] [ rawText (builder.Render d) ] ] ]

let getChordsPage (baseNote: BaseNote) =
    [ getChordsSelect baseNote; getChordsTable baseNote ]
