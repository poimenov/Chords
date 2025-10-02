[<AutoOpen>]
module Chords.Parser

open System
open System.Text.RegularExpressions

/// Преобразование строки в базовую ноту
let parseBaseNote (input: string) =
    match input.Trim().ToUpper() with
    | "A" -> Some A
    | "B" -> Some B
    | "C" -> Some C
    | "D" -> Some D
    | "E" -> Some E
    | "F" -> Some F
    | "G" -> Some G
    | "A#"
    | "A♯" -> Some ASharp
    | "B♭"
    | "BD" -> Some BFlat
    | "C#"
    | "C♯" -> Some CSharp
    | "D♭"
    | "DB" -> Some DFlat
    | "D#"
    | "D♯" -> Some DSharp
    | "E♭"
    | "EB" -> Some EFlat
    | "F#"
    | "F♯" -> Some FSharp
    | "G♭"
    | "GB" -> Some GFlat
    | "G#"
    | "G♯" -> Some GSharp
    | "A♭"
    | "AB" -> Some AFlat
    | _ -> None

/// Парсинг качества аккорда
let parseQuality (input: string) =
    match input.ToUpper() with
    | ""
    | "MAJ" -> Major
    | "M"
    | "MIN" -> Minor
    | "7"
    | "DOM7" -> DominantSeventh
    | "MAJ7"
    | "Δ7"
    | "Δ" -> MajorMajorSeventh
    | "M7"
    | "MIN7" -> MinorSeventh
    | "DIM"
    | "M-5"
    | "°" -> DiminishedTriad
    | "AUG"
    | "+5"
    | "+" -> Augmented
    | "6" -> MajorSixth
    | "M6"
    | "MIN6" -> MinorSixth
    | "7+5"
    | "7#5" -> DominantSeventhSharpFifth
    | "7-5"
    | "7B5" -> DominantSeventhFlatFifth
    | "MM7"
    | "MΔ7"
    | "MINMAJ7" -> MajorMinorSeventh
    | "+7"
    | "AUG7" -> AugmentedSeventh
    | "M7B5"
    | "Ø"
    | "HALFDIM" -> HalfDiminished
    | "DIM7"
    | "°7" -> DiminishedSeventh
    | "MAJ9"
    | "Δ9" -> MajorNinth
    | "M9"
    | "MIN9" -> MinorNinth
    | _ -> Major

/// Основной парсер аккордов
let parseChord (chordString: string) =
    let normalizedString = chordString.Trim()
    // Расширенное регулярное выражение для новых типов аккордов
    let pattern =
        @"^(?<base>[A-Ga-g][#♭b♯♄]?)(?<quality>maj7|maj9|m7|m9|7|6|m6|7\+5|7-5|7#5|7b5|mm7|maj|m|dim|dim7|aug|[+°]|Δ7|Δ9|Δ|\+7|m7b5|Ø)?(?<additional>/[A-Ga-g][#♭b♯♄]?|add\d+|\(\d+\)|.*)?$"

    let m = Regex.Match(normalizedString, pattern, RegexOptions.IgnoreCase)

    if m.Success then
        let baseNote = parseBaseNote m.Groups["base"].Value

        match baseNote with
        | Some baseNote ->
            let quality =
                if m.Groups["quality"].Success then
                    parseQuality m.Groups["quality"].Value
                else
                    Major

            let additional = m.Groups["additional"].Value

            let bassNote =
                if additional.StartsWith("/", StringComparison.OrdinalIgnoreCase) then
                    parseBaseNote (additional.Substring(1))
                else
                    None

            Some
                { BaseNote = baseNote
                  Quality = quality
                  BassNote = bassNote
                  AdditionalInfo =
                    if bassNote.IsSome then
                        additional.Substring(1 + (if bassNote.IsSome then 1 else 0))
                    else
                        additional }
        | None -> None
    else
        None

let parseChordDiagram (name: string, frets: string, fingers: string): ChordDiagram option =
    try
        // Проверяем длину входных строк
        if String.length frets <> 6 || String.length fingers <> 6 then
            None
        else
            let stringStates = 
                List.zip (frets |> Seq.toList) (fingers |> Seq.toList)
                |> List.mapi (fun i (fretChar, fingerChar) -> 
                    match fretChar with
                    | 'x' | 'X' -> Muted
                    | '0' -> Open
                    | fret when System.Char.IsDigit fret ->
                        let fretNum = int (string fret)
                        match fingerChar with
                        | '-' -> 
                            // Если указан лад, но нет пальца, создаем Clamped с пальцем 0
                            Clamped (FretFinger(fretNum, 0))
                        | finger when System.Char.IsDigit finger ->
                            let fingerNum = int (string finger)
                            Clamped (FretFinger(fretNum, fingerNum))
                        | _ -> 
                            // Некорректный символ пальца
                            raise (System.ArgumentException $"Invalid finger character: {fingerChar}")
                    | _ -> 
                        // Некорректный символ лада
                        raise (System.ArgumentException $"Invalid fret character: {fretChar}"))

            // Определяем StartFret - минимальный ненулевой лад или 1 если все струны открытые/приглушенные
            let minFret = 
                stringStates
                |> List.choose (function 
                    | Clamped ff when ff.Fret > 0 -> Some ff.Fret
                    | _ -> None)
                |> function
                    | [] -> 1
                    | frets -> List.min frets

            Some (ChordDiagram(name, minFret, stringStates))
    with
    | _ -> None


/// Форматирование аккорда в читаемый вид
let formatChord chord =
    let baseNoteStr =
        match chord.BaseNote with
        | A -> "A"
        | B -> "B"
        | C -> "C"
        | D -> "D"
        | E -> "E"
        | F -> "F"
        | G -> "G"
        | ASharp -> "A#"
        | BFlat -> "B♭"
        | CSharp -> "C#"
        | DFlat -> "D♭"
        | DSharp -> "D#"
        | EFlat -> "E♭"
        | FSharp -> "F#"
        | GFlat -> "G♭"
        | GSharp -> "G#"
        | AFlat -> "A♭"

    let qualityStr =
        match chord.Quality with
        | Major -> ""
        | Minor -> "m"
        | DominantSeventh -> "7"
        | Augmented -> "+5"
        | DiminishedTriad -> "m-5"
        | MajorSixth -> "6"
        | MinorSixth -> "m6"
        | MajorMajorSeventh -> "maj7"
        | DominantSeventhSharpFifth -> "7+5"
        | DominantSeventhFlatFifth -> "7-5"
        | MajorMinorSeventh -> "mMaj7"
        | MinorSeventh -> "m7"
        | AugmentedSeventh -> "+7"
        | HalfDiminished -> "m7b5"
        | DiminishedSeventh -> "dim7"
        | MajorNinth -> "maj9"
        | MinorNinth -> "m9"

    let bassStr =
        match chord.BassNote with
        | Some bass ->
            let bassNote =
                match bass with
                | A -> "A"
                | B -> "B"
                | C -> "C"
                | D -> "D"
                | E -> "E"
                | F -> "F"
                | G -> "G"
                | ASharp -> "A#"
                | BFlat -> "B♭"
                | CSharp -> "C#"
                | DFlat -> "D♭"
                | DSharp -> "D#"
                | EFlat -> "E♭"
                | FSharp -> "F#"
                | GFlat -> "G♭"
                | GSharp -> "G#"
                | AFlat -> "A♭"

            sprintf "/%s" bassNote
        | None -> ""

    sprintf "%s%s%s%s" baseNoteStr qualityStr chord.AdditionalInfo bassStr

/// Дополнительные утилиты для парсинга
let tryParseChord chordString =
    match parseChord chordString with
    | Some chord -> Ok chord
    | None -> Error $"Unable to recognize the chord: {chordString}"

let parseChords (input: string) =
    input.Split([| ' '; ','; ';' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.choose parseChord
    |> Array.toList
