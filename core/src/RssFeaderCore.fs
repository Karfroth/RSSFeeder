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