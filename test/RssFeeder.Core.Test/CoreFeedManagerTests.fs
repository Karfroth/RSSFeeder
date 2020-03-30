module CoreFeedManager

open Xunit
open Akka.FSharp

type TestDataManager (populateData, actorToReply: Akka.Actor.IActorRef) =

    member private this.Data = 
        if populateData then
            let (typeLevelURL, typeLevelData) = 
                let url = FeedModel.RSSFeedURL (FeedModel.URL "https://typelevel.org/blog/feed.rss")
                (Some(FeedModel.URL "https://typelevel.org/blog/feed.rss"), (FeedModel.getFeedDataAsync url) |> Async.RunSynchronously)
            let (msDevBlogURL, msDevBlogData) = 
                let url = FeedModel.RSSFeedURL (FeedModel.URL "https://devblogs.microsoft.com/dotnet/feed")
                (Some(FeedModel.URL "https://devblogs.microsoft.com/dotnet/feed"), (FeedModel.getFeedDataAsync url) |> Async.RunSynchronously)
            Map.empty.Add(typeLevelURL, typeLevelData).Add(msDevBlogURL, msDevBlogData)
        else Map.empty        

    interface IDataManager.IDataManager<FeedModel.URL option> with

        member this.Add data =
            actorToReply <! "Added"
            async { return data.url }
        member this.Remove key =
            actorToReply <! "Removed"
            async { return () }
        member this.Query key =
            async { return Map.tryFind key this.Data }
        member this.QueryKeys () =
            async { return this.Data |> Map.toSeq |> Seq.map fst }
        member this.Update key data =
            actorToReply <! "Updated"
            async { return () }
        member this.QueryList keys =
            async { return Seq.empty }

type CoreActorTest () =
    inherit Akka.TestKit.Xunit2.TestKit()

let timeout = System.TimeSpan.FromSeconds(20.0) // TODO: Fix this insane timeout
let timeoutNullable = System.Nullable(timeout)

let before populateTestData = 
    let tck = new CoreActorTest ()
    let probe = tck.CreateTestProbe()
    let dispatcher response = probe.TestActor <! response
    let testDataManager = TestDataManager (populateTestData, tck.TestActor)
    let feedManager = RssFeederCore.CoreFeedManager (testDataManager, dispatcher)
    (probe, feedManager)

[<Fact>]
let ``Add works`` () =
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    let (probe, feedManager) = before false
    feedManager.Add (FeedModel.RSSFeedURL url)
    probe.ExpectMsg(RssFeederCore.Added (Some url), timeoutNullable)

[<Fact>]
let ``Remove works`` () =
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    let (probe, feedManager) = before false
    feedManager.Remove url
    probe.ExpectMsg(RssFeederCore.Removed (Some url), timeoutNullable)

[<Fact>]
let ``Update works`` () =
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    let (probe, feedManager) = before false
    feedManager.Update url
    probe.ExpectMsg(RssFeederCore.Updated (Some url), timeoutNullable)

[<Fact>]
let ``UpdateAll works`` () =
    let url = RssFeederCore.Updated (Some(FeedModel.URL "https://typelevel.org/blog/feed.rss")) // TODO: Make this does not depends on external service
    let url2 =  RssFeederCore.Updated (Some(FeedModel.URL "https://devblogs.microsoft.com/dotnet/feed"))
    let (probe, feedManager) = before true
    feedManager.UpdateAll ()
    probe.ExpectMsgAllOf(timeout, [|url; url2|])