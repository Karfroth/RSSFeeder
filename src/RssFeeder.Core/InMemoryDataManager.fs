module InMemoryDataManager
    open IDataManager
    open FeedModel
    
    type InMemoryDataManagerCmd =
    | Add of FeedData * AsyncReplyChannel<int option>
    | Remove of int * AsyncReplyChannel<unit>
    | Query of int * AsyncReplyChannel<FeedData option>
    | QueryAll of AsyncReplyChannel<FeedData seq>
    | Update of int * FeedData * AsyncReplyChannel<unit>
    | QueryList of int seq * AsyncReplyChannel<FeedData seq>

    type InMemoryDataManager () =

        let dataProcessor = MailboxProcessor.Start(fun inbox ->
            let rec loop (data: FeedData array) =
                async { 
                    match! inbox.Receive() with
                    | Add (feedData, replyChannel) ->
                        let ids = seq {
                            for feedData in data do
                                match feedData.id with
                                | Some id -> yield! [|id|]
                                | None -> yield! [||]
                        }
                        let currentID = Some (Seq.last (Seq.sort ids) + 1)
                        let newData = Array.append data [|{ feedData with id = currentID}|]
                        replyChannel.Reply currentID
                        return! loop newData                        
                    | Remove (id, replyChannel) ->
                        let filter = Array.filter (fun x -> 
                            match x.id with
                            | Some dataId -> dataId <> id
                            | None -> false
                        )
                        let newData = filter data
                        replyChannel.Reply ()
                        return! loop newData
                    | Query (id, replyChannel) ->
                        let find = Array.tryFind (fun x -> 
                            match x.id with
                            | Some dataId -> dataId = id
                            | None -> false
                        )
                        replyChannel.Reply (find data)
                        return! loop data
                    | QueryAll replyChannel ->
                        replyChannel.Reply (data)
                        return! loop data
                    | Update (id, feedData, replyChannel) ->
                        let find = Array.tryFind (fun x -> 
                            match x.id with
                            | Some dataId -> dataId = id
                            | None -> false
                        )
                        let existFeedData = find data
                        let updatedData = 
                            Option.map 
                                (fun (oldFeedData: FeedData) ->
                                    let newArticles = 
                                        Seq.fold
                                            (fun (acc: FeedItem seq) feedItem ->
                                                let findResult = 
                                                    Seq.tryFind
                                                        (fun (x: FeedItem) -> x.title = feedItem.title)
                                                        acc
                                                match findResult with
                                                | Some result -> acc
                                                | None -> Seq.append acc (seq { feedItem })
                                            ) Seq.empty (Seq.append oldFeedData.articles feedData.articles)
                                    seq { { oldFeedData with articles = newArticles } }
                                )
                                existFeedData
                        let filter = Array.filter (fun x -> 
                            match x.id with
                            | Some dataId -> dataId <> id
                            | None -> false
                        )
                        let newData = filter data |> Seq.append (Option.defaultValue Seq.empty updatedData) |> Seq.toArray
                        replyChannel.Reply ()
                        return! loop newData
                    | QueryList (ids, replyChannel) ->
                        let toReply = 
                            Seq.filter
                                (fun x -> 
                                    match x.id with
                                    | Some i -> Seq.contains i ids
                                    | None -> false
                                )
                                data
                        replyChannel.Reply toReply
                        return! loop data
                }
            loop Array.empty)

        interface IDataManager<int> with
            member this.Add feedData = 
                dataProcessor.PostAndAsyncReply(fun rc -> Add(feedData, rc) )
            member this.Remove key =
                dataProcessor.PostAndAsyncReply(fun rc -> Remove(key, rc) )
            member this.Query key =
               dataProcessor.PostAndAsyncReply(fun rc -> Query(key, rc) )
            member this.QueryAll () =
                dataProcessor.PostAndAsyncReply QueryAll
            member this.Update key feedData =
                dataProcessor.PostAndAsyncReply(fun rc -> Update (key, feedData, rc) )
            member this.QueryList keys =
                dataProcessor.PostAndAsyncReply(fun rc -> QueryList(keys, rc))