module CoreFeedManager

open Xunit
open FsUnit.Xunit
open Akka.FSharp

type TestDataManager (populateData, actorToReply: Akka.Actor.IActorRef) =

    member private this.Data = 
        if populateData then
            let (typeLevelURL, typeLevelData) = 
                let url = FeedModel.RSSFeedURL (FeedModel.URL "https://typelevel.org/blog/feed.rss")
                (Some(FeedModel.URL "https://typelevel.org/blog/feed.rss"), (FeedModel.getFeedDataAsync url) |> Async.RunSynchronously)
            let (msDevBlogURL, msDevBlogData) = 
                let url = FeedModel.RSSFeedURL (FeedModel.URL "https://devblogs.microsoft.com/feed/landingpage")
                (Some(FeedModel.URL "https://devblogs.microsoft.com/feed/landingpage"), (FeedModel.getFeedDataAsync url) |> Async.RunSynchronously)
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

let timeout = System.Nullable(System.TimeSpan.FromSeconds(20.0)) // TODO: Fix this insane timeout

let before populateTestData dispatcher = 
    let tck = new CoreActorTest ()
    let testDataManager = TestDataManager (populateTestData, tck.TestActor)
    let feedManager = RssFeederCore.CoreFeedManager (testDataManager, dispatcher)
    feedManager

[<Fact>]
let ``Add works`` () =
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    let dispatcher response =
        response |> should equal (RssFeederCore.Added (Some url))
    let feedManager = before false dispatcher
    feedManager.Add (FeedModel.RSSFeedURL url)

[<Fact>]
let ``Remove works`` () =
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    let dispatcher response = 
        response |> should equal (RssFeederCore.Removed (Some url))
    let feedManager = before false dispatcher
    feedManager.Remove url

[<Fact>]
let ``Update works`` () =
    let url = FeedModel.URL "https://typelevel.org/blog/feed.rss" // TODO: Make this does not depends on external service
    let dispatcher response = 
        response |> should equal (RssFeederCore.Updated (Some url))
    let feedManager = before false dispatcher
    feedManager.Update url

[<Fact>]
let ``UpdateAll works`` () =
    let url = RssFeederCore.Updated (Some(FeedModel.URL "https://typelevel.org/blog/feed.rss")) // TODO: Make this does not depends on external service
    let url2 =  RssFeederCore.Updated (Some(FeedModel.URL "https://devblogs.microsoft.com/feed/landingpage"))
    let mutable arr = [|url; url2|]
    let dispatcher response =
        Array.contains response arr |> should equal true
        arr <- Array.filter (fun x -> x <> response) arr
    let feedManager = before false dispatcher
    feedManager.UpdateAll ()