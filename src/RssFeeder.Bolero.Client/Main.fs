module RssFeeder.Bolero.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client
open RssFeederCore
open FeedModel
open Microsoft.AspNetCore.Components
open System.Net.Http

let inMemoryDataStorage = InMemoryDataManager.InMemoryDataManager ()

type FeedData = {title: string; url: string}

type Message =
    | CoreMsg of CoreActorEventMsg
    | UpdateURL of string
    | FeedString of string

type Model = {
    feeds: FeedData seq
    urlInput: string
    feedString: string
}

let feedInfoBox feedData = 
    div [ attr.``class`` "box"] [
        div [ attr.``class`` "media"] [
            div [ attr.``class`` "media-content" ] [
                div [ attr.``class`` "content"] [ 
                    strong [] [text feedData.title]
                    br []
                    text "Content Placeholder"
                    br []
                    time [ attr.datetime "2020-1-1" ] [ text "2020-1-1" ]
                ]
            ]
        ]
    ]

let init () = {feeds = Seq.empty; urlInput = ""; feedString = ""}

let updateModelWithCoreMsg msg model =
    match msg with
    | Added (Some (URL url)) -> 
        { 
        model with
            urlInput = ""
            feeds = Seq.append model.feeds [{title = url; url = url}]
        }
    | Removed (Some (URL url)) -> { model with feeds = Seq.filter (fun feed -> feed.url <> url) model.feeds }
    | _ -> model

let update message model =
    match message with
    | UpdateURL updatedURL-> { model with urlInput = updatedURL }
    | CoreMsg msg -> updateModelWithCoreMsg msg model
    | FeedString str -> { model with feedString = str }

let view (coreActor: CoreFeedManager2<Message>) model dispatch =
    let testHelper () = async {
        let httpClient = new HttpClient()
        let typelevelFeedTask = httpClient.GetAsync "https://typelevel.org/blog/feed.rss"
        let! response = Async.AwaitTask typelevelFeedTask
        let! feedString = Async.AwaitTask (response.Content.ReadAsStringAsync ())
        coreActor.Add dispatch (RSSFeedString {|url = "https://typelevel.org/blog/feed.rss"; body = feedString|})
    }
    div [] [
        div [
            attr.``class`` "column is-one-quarter has-background-primary"
            attr.style "height: 100vh;"
        ] [
            input [
                attr.``type`` "text"
                attr.value model.urlInput
                on.change (fun e -> (dispatch << UpdateURL << unbox) e.Value)
            ]
            button [
                on.click (fun _ -> 
                    testHelper () |> Async.StartImmediate
                )
            ] [text "add"]
            forEach model.feeds feedInfoBox
        ]
        div [
            attr.``class`` "column"
        ] [text model.feedString]
    ]

let initProgram () =
    let httpClient = new HttpClient()
    let coreActor = CoreFeedManager2(inMemoryDataStorage, CoreMsg)
    let sub model = 
        let readTypeLevel dispatch =
            async {
                let typelevelFeedTask = httpClient.GetAsync "https://typelevel.org/blog/feed.rss"
                let! response = Async.AwaitTask typelevelFeedTask
                let! feedString = Async.AwaitTask (response.Content.ReadAsStringAsync ())
                (dispatch << FeedString) feedString
                coreActor.Add dispatch (RSSFeedString {|url = "https://typelevel.org/blog/feed.rss"; body = feedString|})
            } |> Async.StartImmediate
        Cmd.ofSub readTypeLevel
    Program.mkSimple (fun _ -> init ()) update (view coreActor)
    |> Program.withSubscription sub

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program = 
        initProgram ()
#if DEBUG
        |> Program.withHotReload
#endif