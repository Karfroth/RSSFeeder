module FeedTest

open Expecto

let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()

let readRSSFromResource rssName = 
    let asdf = executingAssembly.GetManifestResourceStream("core-test.resources." + rssName)
    let streamReader = new System.IO.StreamReader(asdf)
    let body = streamReader.ReadToEnd ()
    streamReader.Dispose ()
    body

[<Tests>]
let tests =
    testList "FeedTest" [
        testCase "Feed" <| fun _ ->
            let body = readRSSFromResource "typelevel.rss"

            let feedModel = 
                FeedModel.getFeedDataAsync (FeedModel.RSSFeedString body)
                |> Async.RunSynchronously
            Expect.equal feedModel.feed.Title "Typelevel.scala" "feed title should be Typelevel.scala"
            Expect.equal feedModel.url None "feed url should be null"
            Expect.isTrue feedModel.fromFile "feed from file should be true"
    ]