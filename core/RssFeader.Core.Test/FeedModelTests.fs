module FeedModel

open Xunit
open FsUnit.Xunit

let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()

let readRSSFromResource rssName = 
    let asdf = executingAssembly.GetManifestResourceStream("RssFeader.Core.Test.resources." + rssName)
    let streamReader = new System.IO.StreamReader(asdf)
    let body = streamReader.ReadToEnd ()
    streamReader.Dispose ()
    body

[<Fact>]
let ``getFeedDataAsync reads rss file as expected`` () = 
    let body = readRSSFromResource "typelevel.rss"
    let feedModel = 
        FeedModel.getFeedDataAsync (FeedModel.RSSFeedString body)
        |> Async.RunSynchronously
    feedModel.feed.Title |> should equal "Typelevel.scala"
    feedModel.url |> should equal (FeedModel.URL "https://typelevel.org")