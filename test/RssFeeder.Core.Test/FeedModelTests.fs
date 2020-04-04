module FeedModel

open Xunit
open FsUnit.Xunit

let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()

let readRSSFromResource rssName = 
    let resource = executingAssembly.GetManifestResourceStream("RssFeeder.Core.Test.resources." + rssName)
    let streamReader = new System.IO.StreamReader(resource)
    let body = streamReader.ReadToEnd ()
    streamReader.Dispose ()
    body

[<Fact>]
let ``getFeedDataAsync reads rss file as expected`` () = 
    let body = readRSSFromResource "typelevel.rss"
    let feedModel = 
        FeedModel.getFeedDataAsync (FeedModel.RSSFeedString {|body = body; url = ""|})
        |> Async.RunSynchronously
    // feedModel.feed.Title |> should equal "Typelevel.scala"
    feedModel.url |> should equal None

[<Fact>]
let ``getFeedDataAsync reads rss url as expected`` () = 
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Replace real url to something else if possible
    let feedModel = 
        FeedModel.getFeedDataAsync (FeedModel.RSSFeedURL url)
        |> Async.RunSynchronously
    // feedModel.feed.Title |> should equal "Typelevel.scala"
    feedModel.url |> should equal (Some url)