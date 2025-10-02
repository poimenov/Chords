[<AutoOpen>]
module internal Chords.Generator

type Alteration =
    | Sharp
    | Flat
    | DoubleSharp
    | Without

type Fingering =
    struct
        val Alteration: Alteration
        val Strings: StringState list //список струн

        new(alteration, strings) =
            { Alteration = alteration
              Strings = strings }
    end

let getChordDiagram (name: string, startFret: int, fingering: Fingering) =
    let shift =
        match fingering.Alteration with
        | Sharp -> startFret + 1
        | Flat -> startFret - 1
        | DoubleSharp -> startFret + 2
        | Without -> startFret

    //let sFret = if shift = 12 then 1 else shift
    let sFret = if shift >= 12 then shift - 11 else shift

    let shiftString (s: StringState) =
        match s with
        | Clamped f ->
            if shift = 12 then
                if f.Fret = 1 then
                    Open
                else
                    Clamped(FretFinger(f.Fret - 1, f.Finger - 1))
            else
                Clamped f
        | other -> other

    let strings = fingering.Strings |> List.map shiftString
    ChordDiagram(name, sFret, strings)


let baseNoteFrets: Map<BaseNote, int list> =
    [ A, [ 5; 7; 9; 12; 14 ]
      ASharp, [ 6; 8; 10; 13; 15 ]
      BFlat, [ 6; 8; 10; 13; 15 ]
      B, [ 7; 9; 11; 14; 16 ]
      C, [ 8; 10; 12; 3; 5 ]
      CSharp, [ 9; 11; 1; 4; 6 ]
      DFlat, [ 9; 11; 1; 4; 6 ]
      D, [ 10; 12; 2; 5; 7 ]
      DSharp, [ 11; 13; 3; 6; 8 ]
      EFlat, [ 11; 13; 3; 6; 8 ]
      E, [ 12; 14; 4; 7; 9 ]
      F, [ 1; 3; 5; 8; 10 ]
      FSharp, [ 2; 4; 6; 9; 11 ]
      GFlat, [ 2; 4; 6; 9; 11 ]
      G, [ 3; 5; 7; 10; 12 ]
      GSharp, [ 4; 6; 8; 11; 13 ]
      AFlat, [ 4; 6; 8; 11; 13 ] ]
    |> Map.ofList

let chordQualityFingering: Map<ChordQuality, Fingering list> =
    [ // 1. Мажорное трезвучие
      Major,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(3, 3))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(3, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(4, 4))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(3, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 2))
              Muted ]
        ) ]

      // 2. Минорное трезвучие
      Minor,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 2))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Muted
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        ) ]

      // 3. Доминантсептаккорд
      DominantSeventh,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(3, 3))
              Muted
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Muted ]
        ) ]

      // 4. Увеличенное трезвучие
      Augmented,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 4))
              Muted
              Muted ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 4))
              Muted
              Muted ]
        )
        Fingering(
            Sharp,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 4))
              Muted
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Muted ]
        ) ]

      // 5. Уменьшенное трезвучие
      DiminishedTriad,
      [ Fingering(
            Flat,
            [ Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(4, 4))
              Muted
              Muted ]
        )
        Fingering(
            Sharp,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Muted
              Muted ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 2))
              Muted
              Muted ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(3, 2))
              Muted
              Muted ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(5, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        ) ]

      // 6. Мажорное трезвучие с секстой
      MajorSixth,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(3, 3))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1)) ]
        ) ]

      // 7. Минорное трезвучие с секстой
      MinorSixth,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(3, 2))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Sharp,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Muted ]
        )
        Fingering(
            Sharp,
            [ Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(2, 2))
              Muted
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1)) ]
        ) ]

      // 8. Большой мажорный септаккорд
      MajorMajorSeventh,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Muted ]
        ) ]

      // 9. Доминантсептаккорд с повышенной квинтой
      DominantSeventhSharpFifth,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(3, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        )
        Fingering(
            Sharp,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Muted
              Muted ]
        ) ]

      // 10. Доминантсептаккорд с пониженной квинтой
      DominantSeventhFlatFifth,
      [ Fingering(
            Flat,
            [ Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(2, 2))
              Muted
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(3, 2))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        )
        Fingering(
            Sharp,
            [ Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(2, 2))
              Muted
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        ) ]

      // 11. Большой минорный септаккорд
      MajorMinorSeventh,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Muted
              Muted ]
        ) ]

      // 12. Малый минорный септаккорд
      MinorSeventh,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Sharp,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Muted
              Muted ]
        ) ]

      // 13. Увеличенный септаккорд
      AugmentedSeventh,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 4))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Muted
              Muted ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 3))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(2, 2))
              Muted
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Muted ]
        ) ]

      // 14. Полууменьшенный септаккорд
      HalfDiminished,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 4))
              Clamped(FretFinger(2, 4))
              Clamped(FretFinger(2, 4))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        )
        Fingering(
            Sharp,
            [ Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Muted ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Muted ]
        ) ]

      // 15. Уменьшенный септаккорд
      DiminishedSeventh,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        )
        Fingering(
            Sharp,
            [ Clamped(FretFinger(2, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        )
        Fingering(
            Sharp,
            [ Clamped(FretFinger(2, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1)) ]
        ) ]

      // 16. Большой нонаккорд
      MajorNinth,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(3, 4))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(3, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Muted
              Muted ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(2, 4))
              Clamped(FretFinger(2, 4))
              Clamped(FretFinger(2, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(1, 1))
              Muted ]
        ) ]

      // 17. Малый нонаккорд
      MinorNinth,
      [ Fingering(
            Without,
            [ Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(1, 1))
              Clamped(FretFinger(3, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(1, 1)) ]
        )
        Fingering(
            DoubleSharp,
            [ Clamped(FretFinger(2, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Muted ]
        )
        Fingering(
            Flat,
            [ Clamped(FretFinger(2, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 3))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Muted ]
        )
        Fingering(
            Without,
            [ Clamped(FretFinger(4, 4))
              Clamped(FretFinger(1, 1))
              Clamped(FretFinger(2, 2))
              Clamped(FretFinger(4, 3))
              Muted
              Muted ]
        ) ] ]
    |> Map.ofList

let generateChordDiagrams (chord: Chord) : ChordDiagram list =
    match baseNoteFrets |> Map.tryFind chord.BaseNote with
    | Some fs ->
        match chordQualityFingering |> Map.tryFind chord.Quality with
        | Some fgs -> fs |> List.mapi (fun i f -> getChordDiagram (formatChord chord, f, fgs.Item i))
        | None -> []
    | None -> []

let generateChordDiagram (chord: Chord) : ChordDiagram =
    let defaultChordDiagram =
        ChordDiagram(formatChord chord, 1, [ Open; Open; Open; Open; Open; Open ])

    match baseNoteFrets |> Map.tryFind chord.BaseNote with
    | Some fs ->
        match chordQualityFingering |> Map.tryFind chord.Quality with
        | Some fgs ->
            let f = if fs |> List.contains 12 then 12 else fs |> List.min
            let i = fs |> List.findIndex (fun x -> x = f)
            getChordDiagram (formatChord chord, f, fgs.Item i)
        | None -> defaultChordDiagram
    | None -> defaultChordDiagram
