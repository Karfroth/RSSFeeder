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
        | Added of (int option * string * URL)
        | Updated of int
        | Removed of int

    type ReceiveFeed = URL -> Async<RSSFeedDataSource>

    type CoreFeedManager<'a> (receiveFeed: ReceiveFeed, dataManager: IDataManager<int>, box: CoreActorEventMsg -> 'a) =
        inherit IFeedManager<int, FeedModel.URL, 'a>()

        override this.Add dispatch url =
            async {
                let! dataSource = receiveFeed url
                let! feedData = getFeedDataAsync dataSource
                let! result = dataManager.Add feedData
                (Added (result, feedData.feedName, url)) |> box |> dispatch
            } |> Async.StartImmediate
        override this.Remove dispatch key =
            async {
                do! dataManager.Remove key
                (Removed key) |> box |> dispatch
            } |> Async.StartImmediate
        override this.Update dispatch key =
            async { return () } |> ignore
        override this.UpdateAll dispatch () = 
            async { return () } |> ignore
