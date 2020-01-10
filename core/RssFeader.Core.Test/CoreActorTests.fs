module CoreActor

open Xunit
open FsUnit.Xunit
open Akka.FSharp

type TestDataManager () =

    member this.Data = new System.Collections.Generic.Dictionary<string, string>()

    interface IDataManager.IDataManager<FeedModel.URL option> with

        member this.Add data = async { return data.url }
        member this.Remove key = async { return () }
        member this.Query key = async { return None }
        member this.QueryKeys () = async { return Seq.empty }
        member this.Update key data = async { return () }
        member this.QueryList keys = async { return Seq.empty }

let testDataManager = TestDataManager ()

type CoreActorTest () =
    inherit Akka.TestKit.Xunit2.TestKit()

let timeout = System.Nullable(System.TimeSpan.FromSeconds(20.0)) // TODO: Fix this insane timeout

let before () = 
    let tck = new CoreActorTest ()
    let coreActor = spawn tck.Sys "core-actor" (RssFeaderCore.handleCoreCommand testDataManager)
    (tck, coreActor)

[<Fact>]
let ``AddSource Works`` () =
    let (tck, coreActor) = before()
    let source = FeedModel.RSSFeedURL (FeedModel.URL "https://typelevel.org/blog/feed.rss") // TODO: Make this does not depends on external service
    coreActor.Tell(RssFeaderCore.AddSource source, tck.TestActor)

    let expected = RssFeaderCore.Added (Some (FeedModel.URL "https://typelevel.org/blog/feed.rss"))

    tck.ExpectMsg(expected, timeout)

[<Fact>]
let ``UpdateFeed Works`` () =
    let (tck, coreActor) = before()
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    coreActor.Tell(RssFeaderCore.UpdateFeed url, tck.TestActor)

    let expected = RssFeaderCore.Updated (Some url)

    tck.ExpectMsg(expected, timeout)

[<Fact>]
let ``RemoveFeed Works`` () =
    let (tck, coreActor) = before()
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    coreActor.Tell(RssFeaderCore.RemoveFeed url, tck.TestActor)

    let expected = RssFeaderCore.Removed (Some url)

    tck.ExpectMsg(expected, timeout)