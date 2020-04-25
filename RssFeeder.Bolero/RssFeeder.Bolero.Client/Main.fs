module RssFeeder.Bolero.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client
open RssFeederCore
open FeedModel
open Microsoft.AspNetCore.Components
open System.Net.Http
open IDataManager

let inMemoryDataStorage = InMemoryDataManager.InMemoryDataManager () :> IDataManager<int>

type FeedMetaData = {title: string; url: string; id: int}

type Message =
    | CoreMsg of CoreActorEventMsg
    | UpdateURL of string
    | FeedString of string
    | SetCurrentFeed of FeedModel.FeedData option

type Model = {
    feeds: FeedMetaData seq
    urlInput: string
    feedString: string
    currentFeed: FeedModel.FeedData option
}
    

let init () = {feeds = Seq.empty; urlInput = ""; feedString = ""; currentFeed = None}

let updateModelWithCoreMsg msg model =
    match msg with
    | Added (Some id, title, (URL url)) -> 
        { 
        model with
            urlInput = ""
            feeds = Seq.append model.feeds [{title = title; url = url; id = id}]
        }
    | Removed id -> { model with feeds = Seq.filter (fun feed -> feed.id <> id) model.feeds }
    | _ -> model

let update message model =
    match message with
    | UpdateURL updatedURL-> { model with urlInput = updatedURL }
    | CoreMsg msg -> updateModelWithCoreMsg msg model
    | FeedString str -> { model with feedString = str }
    | SetCurrentFeed feedData -> {model with currentFeed = feedData}

let feedMenu dispatch (feedData: FeedMetaData seq) = 
    aside [attr.``class`` "menu"] [
            p [attr.``class`` "menu-label"] [ text "Feeds" ]
            ul [attr.``class`` "menu-list"] [
                forEach feedData (fun data -> 
                    li [] [
                        a [on.click (fun _ -> 
                            async {
                                let! feedData = inMemoryDataStorage.Query data.id
                                do feedData |> SetCurrentFeed |> dispatch
                            } |> Async.StartImmediate
                        )] [text data.title]
                    ]
                )
            ]
        ]

let itemCard (feedItem: FeedItem) =
    div [attr.``class`` "column is-one-third" ] [
        div [ attr.``class`` "card" ] [
            header [attr.``class`` "card-header"] [
                p [attr.``class`` "card-header-title"] [ text feedItem.title ]
            ]
            div [attr.``class`` "card-content"] [
                div [attr.``class`` "content"] [
                    p [attr.``class`` "card-text"] [
                        text feedItem.summary
                    ]
                ]
            ]
        ]
    ]

let view (feedManager: CoreFeedManager<Message>) model dispatch =
    div [ attr.``class`` "columns" ] [
        div [
            attr.``class`` "column is-one-fifth has-background-primary"
        ] [ 
            div [] [
                input [
                    attr.``type`` "text"
                    attr.value model.urlInput
                    on.change (fun e -> (dispatch << UpdateURL << unbox) e.Value)
                ]
                button [
                    on.click (fun _ -> 
                        feedManager.Add dispatch (URL model.urlInput)
                        (dispatch << UpdateURL << unbox) "" 
                    )
                ] [text "add"]
            ]
            feedMenu dispatch model.feeds
        ]
        div [
            attr.``class`` "column"
        ] [
            div [] [
                div [
                    attr.``class`` "columns is-multiline is-mobile"
                ] [
                    match model.currentFeed with
                    | None -> empty
                    | Some feed -> 
                        forEach feed.articles itemCard
                ]
            ]
        ]

    ]

let initProgram () =
    let httpClient = new HttpClient()
    let receiveFeed (targetURL: URL) =
        let (URL url) = targetURL
        async {
            let concatURL = String.concat "/" ["https://cors-anywhere.herokuapp.com"; url]
            printfn "url: %s" concatURL
            let feedTask = httpClient.GetAsync concatURL
            let! response = Async.AwaitTask feedTask
            let! feedString = Async.AwaitTask (response.Content.ReadAsStringAsync ())
            let result: RSSFeedDataSource = {| url = url; body = feedString |}
            return result
        }

    let feedManager = CoreFeedManager(receiveFeed, inMemoryDataStorage, CoreMsg)
    Program.mkSimple (fun _ -> init ()) update (view feedManager)

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program = 
        initProgram ()
#if DEBUG
        |> Program.withHotReload
#endif