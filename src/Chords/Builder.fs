[<AutoOpen>]
module internal Chords.Builder

open System.Xml.Linq
// Создание XML элементов с помощью XElement
let ns = XNamespace.Get "http://www.w3.org/2000/svg"
let getName name = ns + name

let createElement name attributes content =
    let element = XElement(getName name)

    attributes
    |> List.iter (fun (name, value) -> element.SetAttributeValue(XName.Get name, value))

    content |> element.Add
    element

let svgElement attributes content = createElement "svg" attributes content

let rect x y width height rx ry fill stroke strokeWidth =
    createElement
        "rect"
        [ "x", string x
          "y", string y
          "rx", string rx
          "ry", string ry
          "width", string width
          "height", string height
          "fill", fill
          "stroke", stroke
          "stroke-width", string strokeWidth ]
        [||]

let circle cx cy r fill stroke strokeWidth =
    createElement
        "circle"
        [ "cx", string cx
          "cy", string cy
          "r", string r
          "fill", fill
          "stroke", stroke
          "stroke-width", string strokeWidth ]
        [||]

let text x y content fontSize fontWeight fill textAnchor =
    createElement
        "text"
        [ "x", string x
          "y", string y
          "font-size", string fontSize
          "font-weight", fontWeight
          "fill", fill
          "text-anchor", textAnchor ]
        [| XText(string content) |]

let line x1 y1 x2 y2 stroke strokeWidth =
    createElement
        "line"
        [ "x1", string x1
          "y1", string y1
          "x2", string x2
          "y2", string y2
          "stroke", stroke
          "stroke-width", string strokeWidth ]
        [||]

let cross x y z stroke strokeWidth =
    let line1 = line (x - z) y (x + z) y stroke strokeWidth
    let line2 = line x (y - z) x (y + z) stroke strokeWidth
    createElement "g" [ "transform", $"rotate(45, {x}, {y})" ] [| line1; line2 |]

// Константы для размещения элементов
let STRING_SPACING = 16
let FRET_SPACING = 20
let START_X = 24
let START_Y = 24
let FINGER_RADIUS = 6
let BARRE_RADIUS = 4
let FONT_SIZE = 8

// Helper функции для рендеринга
let renderFingerPosition x y fingerNumber backGroundColor foreGroundColor =
    let retVal = [ circle x y FINGER_RADIUS backGroundColor foreGroundColor 1 ]

    if fingerNumber = 0 then
        retVal
    else
        retVal
        @ [ text x (y + 3) (string fingerNumber) FONT_SIZE "normal" foreGroundColor "middle" ]

let renderChordDiagram (chord: ChordDiagram, backGroundColor: string, foreGroundColor: string) =
    let totalWidth = 4 * FRET_SPACING + 40
    let totalHeight = 5 * STRING_SPACING + 40

    let elements = ResizeArray<XElement>()

    // Фон
    elements.Add(rect 0 0 totalWidth totalHeight "auto" "auto" backGroundColor backGroundColor 0)

    // Название аккорда
    elements.Add(text (START_X + FRET_SPACING * 2) 14 chord.Name 14 "bold" foreGroundColor "middle")

    // Струны (горизонтальные линии)
    for stringIndex in 0..5 do
        let y = START_Y + stringIndex * STRING_SPACING
        elements.Add(line START_X y (START_X + 4 * FRET_SPACING) y foreGroundColor 1)

    // Лады (вертикальные линии)
    for fret in 0..4 do
        let x = START_X + fret * FRET_SPACING
        elements.Add(line x START_Y x (START_Y + 5 * STRING_SPACING) foreGroundColor 1)

    if chord.StartFret > 1 then
        elements.Add(
            text
                (START_X + FRET_SPACING / 2)
                (START_Y + 5 * STRING_SPACING + 15)
                (string chord.StartFret)
                FONT_SIZE
                "normal"
                foreGroundColor
                "middle"
        )

    // Обработка струн
    chord.Strings
    |> List.iteri (fun stringIndex state ->
        let y = START_Y + stringIndex * STRING_SPACING

        match state with
        | Open ->
            // Кружок для открытой струны
            elements.Add(circle (START_X - 10) y 5 "none" foreGroundColor 1)
        | Muted ->
            // X для заглушенной струны
            elements.Add(cross (START_X - 10) y 5 foreGroundColor 1)
        | Clamped fret ->
            // elements.Add(
            //     text
            //         (START_X - 10)
            //         (y + (FONT_SIZE + 2) / 2)
            //         fret.Finger
            //         (FONT_SIZE + 2)
            //         "bold"
            //         foreGroundColor
            //         "middle"
            // )

            let cFret = fret.Fret - 1
            let x = START_X + cFret * FRET_SPACING + FRET_SPACING / 2
            elements.AddRange(renderFingerPosition x y fret.Finger backGroundColor foreGroundColor))

    // Создание SVG документа
    let svgDoc =
        svgElement
            [ "viewBox", $"0 0 {totalWidth} {totalHeight}"
              "width", string totalWidth
              "height", string totalHeight ]
            (elements.ToArray() |> Array.map (fun x -> x :> obj))

    // Возвращаем как строку с правильным форматированием
    svgDoc.ToString()

// Helper функции для создания аккордов
let createFret fret finger = FretFinger(fret, finger)
