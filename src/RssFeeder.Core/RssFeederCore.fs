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

    let addSource addSourceActorRef addFeedActorRef source sender mailbox =
        async {
            let! feedData = addSourceActorRef <? source
            let! response = addFeedActorRef <? feedData
            match response with
            | Added _ -> sender <! response
            | _ -> logWarning mailbox "Unexpected response from AddFeedActor"
        } |> Async.StartImmediate

    let updateFeed updateFeedActorRef updateFeedData sender mailbox =
        async {
            let! response = updateFeedActorRef <? updateFeedData
            match response with 
            | Updated _ -> sender <! response
            | _ -> logWarning mailbox "Unexpected respose from UpdateFeedActor"
        } |> Async.StartImmediate

    let updateAll (dataManager: IDataManager<URL option>) self sender =
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

    let removeFeed removeFeedActorRef url sender mailbox =
        async {
            let! response = removeFeedActorRef <? url
            match response with
            | Removed _ -> sender <! response
            | _ -> logWarning mailbox "Unexpected response from RemoveFeedActor"
        } |> Async.StartImmediate

    let handleCoreCommand (dataManager: IDataManager<URL option>) (mailbox: Actor<CoreActorCommandMsg>) =
        let addSourceActorRef = spawn mailbox "add-source-actor" (actorOf2 handleAddSource)
        let addFeedActorRef = spawn mailbox "add-feed-actor" (actorOf2 (handleAddFeed dataManager))
        let updateFeedActorRef = spawn mailbox "update-feed-actor" (actorOf2 (handleUpdateFeed dataManager))
        let removeFeedActorRef = spawn mailbox "remove-feed-actor" (actorOf2 (handleRemoveFeed dataManager))


        let rec loop() = actor {
            match! mailbox.Receive() with
            | AddSource source ->
                let sender = mailbox.Sender()
                addSource addSourceActorRef addFeedActorRef source sender mailbox
                return! loop()
            | UpdateFeed updateFeedData ->
                let sender = mailbox.Sender()
                updateFeed updateFeedActorRef updateFeedData sender mailbox
                return! loop()
            | UpdateAll ->
                let sender = mailbox.Sender()
                updateAll dataManager mailbox.Self sender
                return! loop()
            | RemoveFeed url ->
                let sender = mailbox.Sender()
                removeFeed removeFeedActorRef url sender mailbox
                return! loop()
            | StartAutoUpdate timeout ->
                logWarning mailbox "StartAutoUpdate is not implemented"
                return! loop()
            | StopAutoUpdate ->
                logWarning mailbox "StopAutoUpdate is not implemented"
                return! loop()
        }
        loop()

    type CoreFeedManager<'a> (dataManager: IDataManager<URL option>, box: CoreActorEventMsg -> 'a) =
        inherit IFeedManager<URL, FeedModel.RSSFeedDataSource, 'a>()

        let randID = System.Random().Next().ToString()
        let respond dispatch resultAsync = 
            async {
                let! result = resultAsync
                result |> box |> dispatch
            } |> Async.StartImmediate

        member private this.CoreFeedManagerActor = spawn system ("CoreFeedManager" + randID) (handleCoreCommand dataManager)

        override this.Add dispatch source = respond dispatch (this.CoreFeedManagerActor <? (AddSource source))
        override this.Remove dispatch key = respond dispatch (this.CoreFeedManagerActor <? (RemoveFeed key))
        override this.Update dispatch key = respond dispatch (this.CoreFeedManagerActor <? (UpdateFeed key))
        override this.UpdateAll dispatch () = 
            let now = System.DateTime.Now.Ticks.ToString()
            let rand = System.Random().Next().ToString()
            let updateAllHandler = spawn system ("updateAllHandler" + now + rand) (actorOf (box >> dispatch))
            this.CoreFeedManagerActor.Tell (UpdateAll, updateAllHandler)
