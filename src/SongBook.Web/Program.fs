module SongBook.Web.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Chords

// ---------------------------------
// Models
// ---------------------------------

type Message = { Text: string }

// ---------------------------------
// Views
// ---------------------------------

module Views =
    open Giraffe.ViewEngine

    let header =
        div
            [ _class "header" ]
            [ h1 [] [ encodedText "SongBook.Web" ]
              div
                  [ _class "navbar" ]
                  [ a [ _href "/" ] [ encodedText "Home" ]
                    a [ _href "/chords" ] [ encodedText "Chords" ] ] ]

    let layout (content: XmlNode list) =
        html
            []
            [ head
                  []
                  [ title [] [ encodedText "SongBook.Web" ]
                    link [ _rel "stylesheet"; _type "text/css"; _href "/main.css" ]
                    script [ _src "/main.js" ] [] ]
              body [] [ header; div [ _class "main-content" ] [ yield! content ] ] ]

    let index = [ p [] [ encodedText "Hello from Giraffe!" ] ] |> layout

    let chords (model: Message) =
        let baseNote =
            match parseBaseNote model.Text with
            | Some bn -> bn
            | None -> A

        getChordsPage baseNote |> layout

// ---------------------------------
// Web app
// ---------------------------------

let indexHandler = htmlView Views.index

let chordsHandler (name: string) = htmlView (Views.chords { Text = name })

let webApp =
    choose
        [ GET
          >=> choose
                  [ route "/" >=> indexHandler
                    routex "/chords(/?)" >=> chordsHandler ""
                    routef "/chords/%s" chordsHandler ]
          setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder
        .WithOrigins("http://localhost:5000", "https://localhost:5001")
        .AllowAnyMethod()
        .AllowAnyHeader()
    |> ignore

let configureApp (app: IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()

    (match env.IsDevelopment() with
     | true -> app.UseDeveloperExceptionPage()
     | false -> app.UseGiraffeErrorHandler(errorHandler).UseHttpsRedirection())
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services.AddCors() |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder: ILoggingBuilder) =
    builder.AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot = Path.Combine(contentRoot, "WebRoot")

    Host
        .CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(fun webHostBuilder ->
            webHostBuilder
                .UseContentRoot(contentRoot)
                .UseWebRoot(webRoot)
                .Configure(Action<IApplicationBuilder> configureApp)
                .ConfigureServices(configureServices)
                .ConfigureLogging(configureLogging)
            |> ignore)
        .Build()
        .Run()

    0
