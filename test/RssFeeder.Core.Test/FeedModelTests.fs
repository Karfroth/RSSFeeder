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
        FeedModel.getFeedDataAsync {|body = body; url = ""|}
        |> Async.RunSynchronously
    feedModel.feedName |> should equal "Typelevel.scala"
    feedModel.url |> should equal (FeedModel.URL "")
