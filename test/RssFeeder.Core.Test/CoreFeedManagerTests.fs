module CoreFeedManager

open Xunit
open InMemoryDataManager
open RssFeederCore
open FsUnit.Xunit
open FeedModel

let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()

let readRSSFromResource rssName = 
    let resource = executingAssembly.GetManifestResourceStream("RssFeeder.Core.Test.resources." + rssName)
    let streamReader = new System.IO.StreamReader(resource)
    let body = streamReader.ReadToEnd ()
    streamReader.Dispose ()
    body

let before recieveMock =
    let dataStorate = InMemoryDataManager()
    let feedManager = CoreFeedManager(recieveMock, dataStorate, id)
    feedManager

[<Fact>]
let ``Add works`` () =
    let url = "https://typelevel.org/blog/feed.rss"
    let body = readRSSFromResource "typelevel.rss"
    let feedManager = before (fun _ -> async { return {|url = url; body = body|}})
    let testMsg msg =
        msg |> should equal (Added ((Some << URL) url))
    feedManager.Add testMsg (URL url)

[<Fact>]
let ``Remove works`` () =
    let url = "https://typelevel.org/blog/feed.rss"
    let body = readRSSFromResource "typelevel.rss"
    let feedManager = before (fun _ -> async { return {|url = url; body = body|}})
    let testMsg msg =
        msg |> should equal (Removed ((Some << URL) url))
    feedManager.Remove testMsg (URL url)

// TODO: Enable again
// [<Fact>]
// let ``Update works`` () =
//     let url = "https://typelevel.org/blog/feed.rss"
//     let body = readRSSFromResource "typelevel.rss"
//     let feedManager = before ()
//     let testMsg msg =
//         msg |> should equal (Added ((Some << URL) url))
//     feedManager.Update testMsg (URL url)

// [<Fact>]
// let ``UpdateAll works`` () =
//     let url = RssFeederCore.Updated (Some(FeedModel.URL "https://typelevel.org/blog/feed.rss"))
//     let url2 =  RssFeederCore.Updated (Some(FeedModel.URL "https://devblogs.microsoft.com/dotnet/feed"))
//     let (probe, feedManager, dispatch) = before true
//     feedManager.UpdateAll dispatch ()
//     probe.ExpectMsgAllOf(timeout, [|url; url2|])