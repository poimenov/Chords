# Chords

A project for working with guitar chords. The shared library parses chord symbols, generates fingerings, and renders SVG diagrams. It powers a web application and two desktop applications.

## Features

- Represents chords with a root note, optional bass note, chord quality, and additional information.
- Parses common note and chord symbols, such as `Am`, `Cmaj7`, and `G7`.
- Generates multiple fingering variations for a chord.
- Renders fretboard diagrams showing open, muted, and fretted strings; diagrams are available as SVG.
- Displays fingerings for different chord qualities in the selected key.

## Projects

- `src/Chords` — library for chord types, parsing, fingering generation, and SVG rendering.
- `src/SongBook.Web` — Giraffe web application. The `/chords` page displays chord fingerings, and `/chords/C` shows chords rooted on C.
- `src/SongBook.Photino` — desktop application built with Photino and Blazor.
- `src/SongBook.Avalonia` — desktop application built with Avalonia, featuring a fingering table for the selected root note.

## Requirements

- .NET SDK 10.0

## Build and Run

Build all projects from the repository root:

```bash
dotnet build Chords.slnx
```

Run the web application from its project directory:

```bash
cd src/SongBook.Web
dotnet run
```

Open the address shown in the `dotnet run` output. The chord page is available at `/chords`.

Run either desktop application from the repository root:

```bash
dotnet run --project src/SongBook.Photino
dotnet run --project src/SongBook.Avalonia
```
