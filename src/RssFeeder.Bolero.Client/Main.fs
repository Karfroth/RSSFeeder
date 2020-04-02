module RssFeeder.Bolero.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client
open RssFeederCore
open FeedModel

let inMemoryDataStorage = InMemoryDataManager.InMemoryDataManager ()

type FeedData = {title: string; url: string}

type Message =
    | CoreMsg of CoreActorEventMsg
    | UpdateURL of string

type Model = {
    feeds: FeedData seq
    urlInput: string
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

let init () = {feeds = Seq.empty; urlInput = ""}

let updateModelWithCoreMsg msg model =
    match msg with
    | Added (Some (URL url)) -> { 
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

let view (coreActor: CoreFeedManager<Message>) model dispatch =
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
                    printfn "urlInput is %s" model.urlInput
                    coreActor.Add dispatch (RSSFeedURL (URL model.urlInput))
                )
            ] [text "add"]
            forEach model.feeds feedInfoBox
        ]
        div [
            attr.``class`` "column"
        ] []
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    let coreActor = CoreFeedManager(inMemoryDataStorage, CoreMsg)

    override this.Program = 
        Program.mkSimple (fun _ -> init ()) update (view coreActor)
#if DEBUG
        |> Program.withHotReload
#endif