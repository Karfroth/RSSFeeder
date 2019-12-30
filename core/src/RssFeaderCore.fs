module RssFeaderCore
    
    open IFeedManager
    open FeedModel

    open Akka.FSharp

    let system = System.create "core-actor-system" (Configuration.defaultConfig())

    type CoreActorMsg = 
        | AddSource of FeedModel.RSSFeedDataSource
        | AddFeed of FeedModel.FeedData
        | UpdadeFeed of FeedModel.URL
        | UpdateAll
        | RemoveFeed of FeedModel.URL
        | StartAutoUpdate of int64
        | StopAutoUpdate

    let handleAddSource (mailbox: Actor<FeedModel.RSSFeedDataSource>) msg =
        getFeedDataAsync msg |!> mailbox.Sender()

    let coreActorDef (mailbox: Actor<CoreActorMsg>) =
        let addSourceActorRef = spawn mailbox "add-source-actor" (actorOf2 handleAddSource)
        let rec loop() = actor {
            match! mailbox.Receive() with
            | AddSource source -> 
                addSourceActorRef <! source
                return! loop()
            | AddFeed feedData -> 
                return! loop()
            | UpdadeFeed updateFeed -> 
                return! loop()
            | UpdateAll -> 
                return! loop()
            | RemoveFeed url -> 
                return! loop()
            | StartAutoUpdate timeout ->
                return! loop()
            | StopAutoUpdate -> 
                return! loop()
        }
        loop()