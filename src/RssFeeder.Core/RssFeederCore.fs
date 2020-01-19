module RssFeederCore
    
    open IFeedManager
    open FeedModel

    open IDataManager

    open Akka.FSharp

    let system = System.create "core-actor-system" (Configuration.defaultConfig())

    type CoreActorCommandMsg = 
        | AddSource of FeedModel.RSSFeedDataSource
        | UpdateFeed of FeedModel.URL
        | UpdateAll
        | RemoveFeed of FeedModel.URL
        | StartAutoUpdate of int64
        | StopAutoUpdate

    type CoreActorEventMsg =
        | Added of URL option
        | Updated of URL option
        | Removed of URL option

    let handleAddSource (mailbox: Actor<FeedModel.RSSFeedDataSource>) msg =
        async {
            let! feedData = getFeedDataAsync msg
            return feedData
        } |!> mailbox.Sender()

    let handleAddFeed (dataManager: IDataManager<URL option>) (mailbox: Actor<FeedModel.FeedData>) msg =
        async {
            let feedData = msg
            let! url = dataManager.Add feedData
            return Added url
        } |!> mailbox.Sender()

    let handleUpdateFeed (dataManager: IDataManager<URL option>) (mailbox: Actor<FeedModel.URL>) msg =
        async {
            let! updatedModel = getFeedDataAsync (RSSFeedURL msg)
            let! _ = dataManager.Update (Some msg) updatedModel
            return Updated (Some msg)
        } |!> mailbox.Sender()

    let handleRemoveFeed (dataManager: IDataManager<URL option>) (mailbox: Actor<FeedModel.URL>) msg =
        async {
            let! _ = dataManager.Remove (Some msg)
            return Removed (Some msg)
        } |!> mailbox.Sender()

    let handleCoreCommand (dataManager: IDataManager<URL option>) (mailbox: Actor<CoreActorCommandMsg>) =
        let addSourceActorRef = spawn mailbox "add-source-actor" (actorOf2 handleAddSource)
        let addFeedActorRef = spawn mailbox "add-feed-actor" (actorOf2 (handleAddFeed dataManager))
        let updateFeedActorRef = spawn mailbox "update-feed-actor" (actorOf2 (handleUpdateFeed dataManager))
        let removeFeedActorRef = spawn mailbox "remove-feed-actor" (actorOf2 (handleRemoveFeed dataManager))

        let rec loop() = actor {
            match! mailbox.Receive() with
            | AddSource source ->
                let sender = mailbox.Sender()
                async {
                    let! feedData = addSourceActorRef <? source
                    let! response = addFeedActorRef <? feedData
                    match response with
                    | Added _ -> sender <! response
                    | _ -> logWarning mailbox "Unexpected response from AddFeedActor"
                } |> Async.StartImmediate
                return! loop()
            | UpdateFeed updateFeed ->
                let sender = mailbox.Sender()
                async {
                    let! response = updateFeedActorRef <? updateFeed
                    match response with 
                    | Updated _ -> sender <! response
                    | _ -> logWarning mailbox "Unexpected respose from UpdateFeedActor"
                } |> Async.StartImmediate
                return! loop()
            | UpdateAll ->
                let self = mailbox.Self
                let sender = mailbox.Sender()
                async {
                    let! keys = dataManager.QueryKeys ()
                    Seq.iter (fun kOption ->
                        match kOption with
                        | Some key ->
                            async {
                                let! a = self <? (UpdateFeed key)
                                sender <! a
                            } |> Async.StartImmediate
                        | _ -> ignore ()
                    ) keys
                } |> Async.StartImmediate
                return! loop()
            | RemoveFeed url ->
                let sender = mailbox.Sender()
                async {
                    let! response = removeFeedActorRef <? url
                    match response with
                    | Removed _ -> sender <! response
                    | _ -> logWarning mailbox "Unexpected response from RemoveFeedActor"
                } |> Async.StartImmediate
                return! loop()
            | StartAutoUpdate timeout ->
                logWarning mailbox "StartAutoUpdate is not implemented"
                return! loop()
            | StopAutoUpdate ->
                logWarning mailbox "StopAutoUpdate is not implemented"
                return! loop()
        }
        loop()

    type CoreFeedManager (dataManager: IDataManager<URL option>, dispatcher: CoreActorEventMsg -> unit) =
        inherit IFeedManager<URL, FeedModel.RSSFeedDataSource, CoreActorEventMsg>(dispatcher)

        let randID = System.Random().Next().ToString()
        member private this.CoreFeedManagerActor = spawn system ("CoreFeedManager" + randID) (handleCoreCommand dataManager)
        member private this.EventHandler = spawn system ("EventHandler" + randID) (actorOf (fun msg -> dispatcher(msg)))

        override this.Add source = this.CoreFeedManagerActor.Tell ((AddSource source), this.EventHandler)
        override this.Remove key = this.CoreFeedManagerActor.Tell ((RemoveFeed key), this.EventHandler)
        override this.Update key = this.CoreFeedManagerActor.Tell ((UpdateFeed key), this.EventHandler)
        override this.UpdateAll () = this.CoreFeedManagerActor.Tell (UpdateAll, this.EventHandler)
