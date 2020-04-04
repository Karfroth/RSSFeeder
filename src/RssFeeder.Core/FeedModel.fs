module FeedModel

    open CodeHollow.FeedReader

    type URL = URL of string
    type RSSFeedDataSource = 
        | RSSFeedURL of URL
        | RSSFeedString of {|url: string; body: string|}
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
        // feed: Feed
        // source: RSSFeedDataSource
        articles: FeedItem seq
        url: URL option
    }

    let a = Microsoft.Toolkit.Parsers.Rss.RssParser()
    let b = a.Parse("")
    let d = seq {
        use en = b.GetEnumerator()
        let rec loop () = seq {
            if en.MoveNext() then 
                yield en.Current
                yield! loop () }
        yield! loop () 
    }

    let getFeedFromUrl urlData = 
        let (URL url) = urlData
        FeedReader.ReadAsync url |> Async.AwaitTask
    let getFeedFromString body = 
        async { return FeedReader.ReadFromString body }

    let getFeedFromSource source = 
        match source with
        | RSSFeedURL url -> getFeedFromUrl url
        | RSSFeedString body -> getFeedFromString body.body

    let getFeedDataAsync feedSource = 
        async {
            match feedSource with
            | RSSFeedURL _ ->
                let! feed = getFeedFromSource feedSource
                let url = 
                    match feedSource with
                    | RSSFeedURL u -> Some u
                    | _ -> Some (URL feed.Link)
                return {
                    lastSyncTime = System.DateTime.Now.Ticks
                    // source = feedSource
                    // feed = feed
                    url = url
                    articles = feed.Items
                }
            | RSSFeedString body ->
                let stringReader = new System.IO.StringReader(body.body)
                let xmlReader = System.Xml.XmlReader.Create(stringReader)
                let syndication = System.ServiceModel.Syndication.SyndicationFeed.Load(xmlReader)
                return {
                    lastSyncTime = System.DateTime.Now.Ticks
                    // source = feedSource
                    // feed = Seq.empty<FeedItem>
                    url = (Some << URL) (body.url)
                    articles = Seq.empty
                }
        }