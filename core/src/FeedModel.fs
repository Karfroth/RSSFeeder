module FeedModel

    open CodeHollow.FeedReader

    type URL = URL of string
    type RSSFeedDataSource = 
        | RSSFeedURL of URL
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
        url: URL
    }

    let getFeedFromUrl urlData = 
        let (URL url) = urlData
        FeedReader.ReadAsync url |> Async.AwaitTask
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
                url = URL feed.Link
                articles = feed.Items
            }
        }