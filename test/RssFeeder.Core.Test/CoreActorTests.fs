module CoreActor

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
    let testDataManager = TestDataManager (populateTestData, probe)
    let coreActor = spawn tck.Sys "core-actor" (RssFeederCore.handleCoreCommand testDataManager)
    (tck, coreActor, probe)

[<Fact>]
let ``AddSource Works`` () =
    let (tck, coreActor, probe) = before false
    let source = FeedModel.RSSFeedURL (FeedModel.URL "https://typelevel.org/blog/feed.rss") // TODO: Make this does not depends on external service
    coreActor.Tell(RssFeederCore.AddSource source, tck.TestActor)

    let expected = RssFeederCore.Added (Some (FeedModel.URL "https://typelevel.org/blog/feed.rss"))

    probe.ExpectMsg("Added", timeoutNullable) |> ignore
    tck.ExpectMsg(expected, timeoutNullable)

[<Fact>]
let ``UpdateFeed Works`` () =
    let (tck, coreActor, probe) = before false
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    coreActor.Tell(RssFeederCore.UpdateFeed url, tck.TestActor)

    let expected = RssFeederCore.Updated (Some url)

    probe.ExpectMsg("Updated", timeoutNullable) |> ignore
    tck.ExpectMsg(expected, timeoutNullable)

[<Fact>]
let ``UpdateAll Works`` () =
    let (tck, coreActor, probe) = before true
    coreActor.Tell(RssFeederCore.UpdateAll, tck.TestActor)

    let expected1 = RssFeederCore.Updated (Some(FeedModel.URL "https://typelevel.org/blog/feed.rss"))
    let expected2 = RssFeederCore.Updated (Some(FeedModel.URL "https://devblogs.microsoft.com/dotnet/feed"))

    probe.ExpectMsgAllOf(timeout, [|"Updated"; "Updated"|]) |> ignore
    tck.ExpectMsgAllOf(timeout, [|expected1; expected2|])

[<Fact>]
let ``RemoveFeed Works`` () =
    let (tck, coreActor, probe) = before false
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    coreActor.Tell(RssFeederCore.RemoveFeed url, tck.TestActor)

    probe.ExpectMsg("Removed", timeoutNullable) |> ignore
    let expected = RssFeederCore.Removed (Some url)

    tck.ExpectMsg(expected, timeoutNullable)