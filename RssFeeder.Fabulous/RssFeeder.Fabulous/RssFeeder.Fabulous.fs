﻿// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace RssFeeder.Fabulous

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

module App = 
    open RssFeederCore
    open FeedModel
    open System.Net.Http
    open IDataManager
    
    let getDataStorage dbPathOpt =
        match dbPathOpt with
        | Some dbPath -> SQLiteStorage.SQLiteDataManager (dbPath) :> IDataManager<int>
        | None -> InMemoryDataManager.InMemoryDataManager () :> IDataManager<int>
    
    type FeedMetaData = {title: string; url: string; id: int}
    
    type Message =
        | CoreMsg of CoreActorEventMsg
        | UpdateURL of string
        | FeedString of string
        | SetCurrentFeed of FeedModel.FeedData option
        | Initialization of FeedMetaData seq
    
    type Model = {
        feeds: FeedMetaData seq
        urlInput: string
        feedString: string
        currentFeed: FeedModel.FeedData option
    }

    let init (dataStorage: IDataManager<int>) () =
        let feedMetaDataSeq = async {
            let! allData = dataStorage.QueryAll ()
            return Initialization (seq {
                for data in allData do
                    match (data.url, data.id) with
                    | (URL url, Some id) -> yield {title = data.feedName; id = id; url = url}
                    | _ -> ()
            })
        }
        {feeds = Seq.empty; urlInput = ""; feedString = ""; currentFeed = None}, Cmd.ofAsyncMsg feedMetaDataSeq
    
    let updateModelWithCoreMsg msg model =
        match msg with
        | Added (Some id, title, URL url) -> 
            { 
            model with
                urlInput = ""
                feeds = Seq.append model.feeds [{title = title; url = url; id = id}]
            }
        | Removed id -> { model with feeds = Seq.filter (fun feed -> feed.id <> id) model.feeds }
        | _ -> model
    
    let update message model =
        match message with
        | UpdateURL updatedURL-> { model with urlInput = updatedURL }, Cmd.none
        | CoreMsg msg -> updateModelWithCoreMsg msg model, Cmd.none
        | FeedString str -> { model with feedString = str }, Cmd.none
        | SetCurrentFeed feedData -> { model with currentFeed = feedData }, Cmd.none
        | Initialization feedData -> { model with feeds = feedData }, Cmd.none
    
    let feedsPage feeds = 
        View.ContentPage(
            content= View.StackLayout(
                padding = Thickness 10.0,
                children =
                    match feeds with
                    | Some currentFeed ->
                        List.map (fun (x: FeedModel.FeedItem) -> View.Label x.title) (Seq.toList currentFeed.articles)
                    | None -> []
            )
        )

    let feedDetailPage (currentFeed: FeedData option) = 
        let feedCard (x: FeedModel.FeedItem) =
            let cell = 
                View.ViewCell(
                    view = View.StackLayout(
                        children = [
                            View.Label(text = x.title, isVisible = true, textColor=Color.Accent, lineBreakMode=LineBreakMode.TailTruncation)
                            View.Label(text = x.summary, lineBreakMode = LineBreakMode.TailTruncation, maxLines = 3)
                        ],
                        margin = Thickness 10.0
                    )
                )
            (cell, Seq.tryHead x.links)

        let onSelected urls idx = 
            match Option.bind id (Option.bind (fun i -> List.tryItem i urls) idx) with
            | Some (URL url) -> Async.AwaitTask (Xamarin.Essentials.Browser.OpenAsync(url)) |> Async.StartImmediate
            | None -> ()

        let (title, items, urls) = 
            match currentFeed with
                | Some feed -> 
                    let (items, urls) = List.unzip (List.map feedCard (Seq.toList feed.articles))
                    (feed.feedName, items, urls)
                | None -> ("RSSFeeder", [], [])

        View.NavigationPage(
            pages=[
                View.ContentPage(
                    title = title,
                    content = View.StackLayout(
                        padding = Thickness 10.0,
                        children = [
                            View.ListView(
                                rowHeight = 100,
                                items = items,
                                itemSelected = onSelected urls
                            )
                        ]
                    )
                )
            ]
        )

    let view (dataStorage: IDataManager<int>) (feedManager: CoreFeedManager<Message>) model dispatch =
        let addFeed _ =
             feedManager.Add dispatch (URL model.urlInput)
             (dispatch << UpdateURL) ""
        let feeds = 
            View.ListView(
                items = List.map (fun x -> View.TextCell x.title) (Seq.toList model.feeds),
                itemSelected = (fun idx ->
                    match Option.bind (fun i -> Seq.tryItem i model.feeds) idx with
                    | Some item ->
                        async {
                            let! feedData = dataStorage.Query item.id
                            do feedData |> SetCurrentFeed |> dispatch
                        } |> Async.StartImmediate
                    | _ -> ()
                )
            )
        View.MasterDetailPage(
            masterBehavior = MasterBehavior.Default,
            master = View.ContentPage(
                title = "Master Title",
                useSafeArea = true,
                content = View.StackLayout(
                    children=[
                        View.Entry(
                            text = model.urlInput,
                            textChanged = (fun t -> (dispatch << UpdateURL) t.NewTextValue),
                            completed = (addFeed)
                        )
                        feeds
                    ]
                )
            ),
            detail = feedDetailPage model.currentFeed
        )

    let program dbPathOpt =
        let httpClient = new HttpClient()
        let receiveFeed (targetURL: URL) =
            let (URL url) = targetURL
            async {
                let concatURL = String.concat "/" ["https://cors-anywhere.herokuapp.com"; url]
                printfn "url: %s" concatURL
                let request = new HttpRequestMessage(new HttpMethod("GET"), concatURL)
                request.Headers.TryAddWithoutValidation("X-Requested-With", "XMLHttpRequest") |> ignore
                let feedTask = httpClient.SendAsync request
                let! response = Async.AwaitTask feedTask
                let! feedString = Async.AwaitTask (response.Content.ReadAsStringAsync ())
                let result: RSSFeedDataSource = {| url = url; body = feedString |}
                return result
            }
    
        let dataStorage = getDataStorage dbPathOpt
        let feedManager = CoreFeedManager(receiveFeed, dataStorage, CoreMsg)
        Program.mkProgram (init dataStorage) update (view dataStorage feedManager)

type App (dbPathOpt: string option) as app = 
    inherit Application ()

    let runner = 
        App.program (dbPathOpt)
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/tools.html#live-update for further  instructions.
    //
    do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/models.html#saving-application-state for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


