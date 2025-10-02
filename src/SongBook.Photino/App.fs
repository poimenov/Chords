[<AutoOpen>]
module SongBook.Photino.App

open System
open Microsoft.AspNetCore.Components.Web
open Microsoft.AspNetCore.Components.Routing
open Microsoft.FluentUI.AspNetCore.Components
open FSharp.Data.Adaptive
open Fun.Blazor
open Fun.Blazor.Router
open Chords
open Microsoft.FSharp.Reflection


type IShareStore with
    member store.IsMenuOpen = store.CreateCVal(nameof store.IsMenuOpen, true)

let homePage =
    fragment {
        SectionContent'' {
            SectionName "Title"
            "Home"
        }

        FluentLabel'' {
            Typo Typography.H1
            Color Color.Accent
            "Hi from FunBlazor"
        }
    }

let chordFingering =
    fragment {
        SectionContent'' {
            SectionName "Title"
            "Аппликатуры аккордов"
        }

        FluentLabel'' {
            Typo Typography.H1
            Color Color.Accent
            "Аппликатуры аккордов"
        }

        adapt {
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

            let! selectedBaseNote, setSelectedBaseNote = cval(BaseNote.A).WithSetter()
            let builder = ChordBuilder("white", "black")
            let getDiagrams (chord: Chord) = builder.GetChordDiagrams chord

            div {
                FluentSelect'' {
                    Width "100px"
                    Items allBaseNotes
                    type' typeof<BaseNote>
                    Label "Основной тон аккорда"
                    OptionText(fun x -> Convert.ToString x)
                    OptionValue(fun x -> Convert.ToString x)
                    SelectedOptionChanged(fun x -> setSelectedBaseNote x)
                    Value(Convert.ToString selectedBaseNote)
                }
            }

            for q in allChordQualities do
                div {
                    class' "akkords-list"

                    div { getChordQualityNames q }

                    let chord =
                        { BaseNote = selectedBaseNote
                          Quality = q
                          BassNote = None
                          AdditionalInfo = "" }

                    let diagrams = getDiagrams chord

                    for d in diagrams do
                        div {
                            class' "akkord"
                            html.raw (builder.Render d)
                        }
                }
        }
    }

let appHeader =
    FluentHeader'' {
        FluentStack'' {
            Orientation Orientation.Horizontal

            img {
                src "favicon.ico"

                style { height "28px" }
            }

            FluentLabel'' {
                Typo Typography.H2
                Color Color.Fill
                "SongBook"
            }
        }
    }

let appFooter =
    html.inject (fun (los: ILinkOpeningService) ->
        FluentFooter'' {
            FluentAnchor'' {
                Appearance Appearance.Hypertext
                href "#"
                OnClick(fun _ -> los.OpenUrl "https://slaveoftime.github.io/Fun.Blazor.Docs/")

                "Fun.Blazor"
            }

            FluentSpacer''

            FluentAnchor'' {
                Appearance Appearance.Hypertext
                href "#"
                OnClick(fun _ -> los.OpenUrl "https://www.tryphotino.io")

                "Photino"
            }
        })

let navmenus =
    html.injectWithNoKey (fun (store: IShareStore) ->
        adaptiview () {
            let! binding = store.IsMenuOpen.WithSetter()

            FluentNavMenu'' {
                Width 200
                Collapsible true
                Expanded' binding

                FluentNavLink'' {
                    Href "/"
                    Match NavLinkMatch.All
                    Icon(Icons.Regular.Size20.Home())
                    "Home"
                }

                FluentNavLink'' {
                    Href "/chords"
                    Match NavLinkMatch.Prefix
                    Icon(Icons.Regular.Size20.MusicNote1())
                    "Chords"
                }
            }
        })

let routes = html.route [| routeCi "/chords" chordFingering; routeAny homePage |]

let app =
    ErrorBoundary'' {
        ErrorContent(fun e ->
            FluentLabel'' {
                Color Color.Error
                string e
            })

        FluentToastProvider''

        FluentLayout'' {
            appHeader

            FluentStack'' {
                Width "100%"
                class' "main"
                Orientation Orientation.Horizontal
                navmenus

                FluentBodyContent'' {
                    class' "body-content"
                    style { overflowHidden }

                    div {
                        class' "content"

                        routes
                    }
                }
            }

            appFooter
        }
    }

type App() =
    inherit FunComponent()

    override _.Render() = app
