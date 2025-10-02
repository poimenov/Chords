namespace Chords

// Типы данных для представления аппликатуры
type FretFinger =
    struct
        val Fret: int //лад прижатия
        val Finger: int //палец прижатия

        new(fret, finger) =
            { Fret =
                if fret > 0 && fret < 20 then
                    fret
                else
                    raise (System.ArgumentException $"Expected digit between 1 and 20 , received {fret}")
              Finger = if finger > 0 && finger < 5 then finger else 0 }
    end

type StringState =
    | Open //открытая струна
    | Muted //приглушённая струна
    | Clamped of FretFinger //струна зажатая на определённом ладу определённым пальцем


type ChordDiagram =
    struct
        val Name: string //название аккрода
        val StartFret: int //первый отображаемый в диаграме лад
        val Strings: StringState list //список струн

        new(name, startFret, strings) =
            { Name = name
              StartFret = startFret
              Strings =
                strings
                |> fun items ->
                    if List.length items = 6 then
                        items
                    else
                        raise (System.ArgumentException $"Expected 6 items, received {List.length items}") }
    end

/// Типы аккордов
type ChordQuality =
    | Major // Мажорное трезвучие
    | Minor // Минорное трезвучие /m/
    | DominantSeventh // Доминантсептаккорд /7/
    | Augmented // Увеличенное трезвучие /+5/
    | DiminishedTriad // Уменьшенное трезвучие /m³/ (на самом деле dim или °)
    | MajorSixth // Мажорное трезвучие с секстой /6/
    | MinorSixth // Минорное трезвучие с секстой /m6/
    | MajorMajorSeventh // Большой мажорный септаккорд /maj7/
    | DominantSeventhSharpFifth // Доминантсептаккорд с повышенной квинтой /7+5/
    | DominantSeventhFlatFifth // Доминантсептаккорд с пониженной квинтой /7-5/
    | MajorMinorSeventh // Большой минорный септаккорд /mMaj7/
    | MinorSeventh // Малый минорный септаккорд /m7/
    | AugmentedSeventh // Увеличенный септаккорд /+7/+5/
    | HalfDiminished // Полууменьшенный септаккорд /m7b5/
    | DiminishedSeventh // Уменьшенный септаккорд /dim7/
    | MajorNinth // Большой нонаккорд /maj9/
    | MinorNinth // Малый нонаккорд /m9/

/// Базовая нота аккорда
type BaseNote =
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | ASharp
    | BFlat
    | CSharp
    | DFlat
    | DSharp
    | EFlat
    | FSharp
    | GFlat
    | GSharp
    | AFlat

/// Полный аккорд
type Chord =
    { BaseNote: BaseNote
      Quality: ChordQuality
      BassNote: BaseNote option
      AdditionalInfo: string }
