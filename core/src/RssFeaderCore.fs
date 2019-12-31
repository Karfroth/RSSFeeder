module RssFeaderCore
    
    open IFeedManager
    open FeedModel

    open IDataManager

    open Akka.FSharp

    let system = System.create "core-actor-system" (Configuration.defaultConfig())

    type CoreActorCommandMsg = 
        | AddSource of FeedModel.RSSFeedDataSource
        | AddFeed of FeedModel.FeedData
        | UpdateFeed of FeedModel.URL
        | UpdateAll
        | RemoveFeed of FeedModel.URL
        | StartAutoUpdate of int64
        | StopAutoUpdate

    type CoreActorEventMsg =
        | Added of URL 
        | Updated of URL
        | Removed of URL

    let handleAddSource (mailbox: Actor<FeedModel.RSSFeedDataSource>) msg =
        async {
            let! feedData = getFeedDataAsync msg
            return AddFeed feedData
        } |!> mailbox.Sender()

    let handleAddFeed (dataManager: IDataManager<URL>) (mailbox: Actor<FeedModel.FeedData>) msg =
        async {
            let! url = dataManager.Add msg
            return Added url
        } |!> mailbox.Sender()

    let handleUpdateFeed (dataManager: IDataManager<URL>) (mailbox: Actor<FeedModel.URL>) msg =
        async {
            let! updatedModel = getFeedDataAsync (RSSFeedURL msg)
            let! _ = dataManager.Update msg updatedModel
            return Updated msg
        } |!> mailbox.Sender()

    let handleRemoveFeed (dataManager: IDataManager<URL>) (mailbox: Actor<FeedModel.URL>) msg =
        async {
            let! _ = dataManager.Remove msg
            return Removed msg
        } |!> mailbox.Sender()   

    let handleCoreCommand (dataManager: IDataManager<URL>) (mailbox: Actor<CoreActorCommandMsg>) =
        let addSourceActorRef = spawn mailbox "add-source-actor" (actorOf2 handleAddSource)
        let addFeedActorRef = spawn mailbox "add-feed-actor" (actorOf2 (handleAddFeed dataManager))
        let updateFeedActorRef = spawn mailbox "update-feed-actor" (actorOf2 (handleUpdateFeed dataManager))
        let removeFeedActorRef = spawn mailbox "remove-feed-actor" (actorOf2 (handleRemoveFeed dataManager))

        let rec loop() = actor {
            match! mailbox.Receive() with
            | AddSource source -> 
                addSourceActorRef <! source
                return! loop()
            | AddFeed feedData -> 
                let sender = mailbox.Sender()
                async {
                    let! response = addFeedActorRef <? feedData
                    match response with
                    | Added _ -> sender <! response
                    | _ -> logWarning mailbox "Unexpected response from AddFeedActor"
                } |> ignore
                return! loop()
            | UpdateFeed updateFeed ->
                let sender = mailbox.Sender()
                async {
                    let! response = updateFeedActorRef <? updateFeed
                    match response with 
                    | Updated _ -> sender <! response
                    | _ -> logWarning mailbox "Unexpected respose from UpdateFeedActor"
                } |> ignore
                return! loop()
            | UpdateAll ->
                return! loop()
            | RemoveFeed url ->
                let sender = mailbox.Sender()
                async {
                    let! response = removeFeedActorRef <? url
                    match response with
                    | Removed _ -> sender <! response
                    | _ -> logWarning mailbox "Unexpected response from RemoveFeedActor"
                } |> ignore
                return! loop()
            | StartAutoUpdate timeout ->
                return! loop()
            | StopAutoUpdate ->
                return! loop()
        }
        loop()