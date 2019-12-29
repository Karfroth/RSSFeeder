module RssFeaderCore
    
    open IFeedManager
    open FeedModel

    open Akka.FSharp

    let system = System.create "core-actor-system" (Configuration.defaultConfig())

