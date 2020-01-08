module CoreActor

open Xunit
open FsUnit.Xunit
open Akka.FSharp

type TestDataManager () =

    member this.Data = new System.Collections.Generic.Dictionary<string, string>()

    interface IDataManager.IDataManager<FeedModel.URL> with

        member this.Add data = async {
            let (FeedModel.URL url) = data.url
            this.Data.Add(url, url)
            return data.url 
        }
        member this.Remove key = async { return () }
        member this.Query key = async { return None }
        member this.QueryKeys () = async { return Seq.empty }
        member this.Update key data = async { return () }
        member this.QueryList keys = async { return Seq.empty }

let testDataManager = TestDataManager ()

type CoreActorTest () =
    inherit Akka.TestKit.Xunit2.TestKit()

[<Fact>]
let ``Add Source Works`` () =
    let tck = new CoreActorTest ()
    let coreActor = spawn tck.Sys "core-actor" (RssFeaderCore.handleCoreCommand testDataManager)
    let source = FeedModel.RSSFeedURL (FeedModel.URL "https://typelevel.org/blog/feed.rss") // TODO: Make this does not depends on external service
    coreActor.Tell(RssFeaderCore.AddSource source, tck.TestActor)

    let expected = RssFeaderCore.Added (FeedModel.URL "https://typelevel.org")

    tck.ExpectMsg(expected, System.Nullable(System.TimeSpan.FromSeconds(10.0)))
