module FeedModel

    open System.ServiceModel.Syndication

    type URL = URL of string
    type RSSFeedDataSource = {|url: string; body: string|}

    type FeedItem = {title: string; summary: string; authors: string seq; date: string; links: URL seq}

    type FeedData = { 
        lastSyncTime: int64
        feedName: string
        articles: FeedItem seq
        url: URL
        id: int option
    }

    let removeTag str = 
        let tagRemoved = System.Text.RegularExpressions.Regex.Replace(str, "<.*?>", "")
        System.Text.RegularExpressions.Regex.Replace(tagRemoved, "\n\n", "\n")
        |> System.String.Concat

    let getFeedDataAsync (feedSource: RSSFeedDataSource) = 
        async {
            let stringReader = new System.IO.StringReader(feedSource.body)
            let xmlReader = System.Xml.XmlReader.Create(stringReader)
            let syndication = SyndicationFeed.Load(xmlReader)
            let articles = 
                seq {
                    for item in syndication.Items do
                        yield {
                            title = item.Title.Text
                            summary = removeTag item.Summary.Text
                            authors = Seq.map (fun (x: SyndicationPerson) -> x.Name) item.Authors
                            date = item.LastUpdatedTime.Ticks.ToString()
                            links = Seq.map (fun (x: SyndicationLink) -> URL (x.Uri.ToString ())) item.Links
                        }
                }
            return {
                lastSyncTime = System.DateTime.Now.Ticks
                feedName = syndication.Title.Text
                url = URL feedSource.url
                articles = articles
                id = None
            }
        }