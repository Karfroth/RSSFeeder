module RssFeederCore
    
    open IFeedManager
    open FeedModel

    open IDataManager

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

    type ReceiveFeed = URL -> Async<RSSFeedDataSource>

    type CoreFeedManager<'a> (receiveFeed: ReceiveFeed, dataManager: IDataManager<URL option>, box: CoreActorEventMsg -> 'a) =
        inherit IFeedManager<URL, FeedModel.RSSFeedDataSource, 'a>()

        override this.Add dispatch url =
            async {
                let! dataSource = receiveFeed url
                let! feedData = getFeedDataAsync dataSource
                let! result = dataManager.Add feedData
                (Added result) |> box |> dispatch
            } |> Async.StartImmediate
        override this.Remove dispatch key =
            async {
                do! dataManager.Remove (Some key)
                (Removed (Some key)) |> box |> dispatch
            } |> Async.StartImmediate
        override this.Update dispatch key =
            async { return () } |> ignore
        override this.UpdateAll dispatch () = 
            async { return () } |> ignore
