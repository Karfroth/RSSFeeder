module CoreActor

open Xunit
open FsUnit.Xunit
open Akka.FSharp

type TestDataManager (actorToReply: Akka.Actor.IActorRef) =

    member this.Data = new System.Collections.Generic.Dictionary<string, string>()

    interface IDataManager.IDataManager<FeedModel.URL option> with

        member this.Add data =
            actorToReply <! "Added"
            async { return data.url }
        member this.Remove key =
            actorToReply <! "Removed"
            async { return () }
        member this.Query key =
            async { return None }
        member this.QueryKeys () =
            async { return Seq.empty }
        member this.Update key data =
            actorToReply <! "Updated"
            async { return () }
        member this.QueryList keys =
            async { return Seq.empty }

type CoreActorTest () =
    inherit Akka.TestKit.Xunit2.TestKit()

let timeout = System.Nullable(System.TimeSpan.FromSeconds(20.0)) // TODO: Fix this insane timeout

let before () = 
    let tck = new CoreActorTest ()
    let probe = tck.CreateTestProbe()
    let testDataManager = TestDataManager (probe)
    let coreActor = spawn tck.Sys "core-actor" (RssFeaderCore.handleCoreCommand testDataManager)
    (tck, coreActor, probe)

[<Fact>]
let ``AddSource Works`` () =
    let (tck, coreActor, probe) = before()
    let source = FeedModel.RSSFeedURL (FeedModel.URL "https://typelevel.org/blog/feed.rss") // TODO: Make this does not depends on external service
    coreActor.Tell(RssFeaderCore.AddSource source, tck.TestActor)

    let expected = RssFeaderCore.Added (Some (FeedModel.URL "https://typelevel.org/blog/feed.rss"))

    probe.ExpectMsg("Added", timeout) |> ignore
    tck.ExpectMsg(expected, timeout)

[<Fact>]
let ``UpdateFeed Works`` () =
    let (tck, coreActor, probe) = before()
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    coreActor.Tell(RssFeaderCore.UpdateFeed url, tck.TestActor)

    let expected = RssFeaderCore.Updated (Some url)

    probe.ExpectMsg("Updated", timeout) |> ignore
    tck.ExpectMsg(expected, timeout)

[<Fact>]
let ``RemoveFeed Works`` () =
    let (tck, coreActor, probe) = before()
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    coreActor.Tell(RssFeaderCore.RemoveFeed url, tck.TestActor)

    probe.ExpectMsg("Removed", timeout) |> ignore
    let expected = RssFeaderCore.Removed (Some url)

    tck.ExpectMsg(expected, timeout)