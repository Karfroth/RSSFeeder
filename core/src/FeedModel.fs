module FeedModel

    open CodeHollow.FeedReader


    type RSSFeedDataSource = 
        | RSSFeedURL of string
        | RSSFeedString of string
    module RSSFeedDataSource =
        let isRSSFeedURL source = 
            match source with
            | RSSFeedURL _ -> true
            | _ -> false
        let isRSSFeedString source = 
            match source with
            | RSSFeedString _ -> true
            | _ -> false

    type FeedData = { 
        lastSyncTime: int64
        feed: Feed
        source: RSSFeedDataSource
        articles: FeedItem seq
    }

    let getFeedFromUrl url = FeedReader.ReadAsync url |> Async.AwaitTask
    let getFeedFromString body = async { return FeedReader.ReadFromString body }

    let getFeedFromSource source = 
        match source with
        | RSSFeedURL url -> getFeedFromUrl url
        | RSSFeedString body -> getFeedFromString body

    let getFeedDataAsync feedSource = 
        async {
            let! feed = getFeedFromSource feedSource
            return {
                lastSyncTime = System.DateTime.Now.Ticks
                source = feedSource
                feed = feed
                articles = feed.Items
            }
        }