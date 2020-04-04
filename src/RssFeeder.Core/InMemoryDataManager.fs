module InMemoryDataManager
    open IDataManager
    open FeedModel
    
    type InMemoryDataManagerCmd =
    | Add of FeedData * AsyncReplyChannel<URL option>
    | Remove of URL * AsyncReplyChannel<unit>
    | Query of URL * AsyncReplyChannel<FeedData option>
    | QueryKeys of AsyncReplyChannel<URL option seq>
    | Update of URL * FeedData * AsyncReplyChannel<unit>
    | QueryList of URL option seq * AsyncReplyChannel<FeedData seq>

    type InMemoryDataManager () =

        let dataProcessor = MailboxProcessor.Start(fun inbox ->
            let rec loop (data: Map<URL, FeedData>) =
                async { 
                    match! inbox.Receive() with
                    | Add (feedData, replyChannel) ->
                        let newData = Map.add feedData.url feedData data
                        replyChannel.Reply (Some feedData.url)
                        return! loop newData                        
                    | Remove (url, replyChannel) ->
                        let newData = Map.remove url data
                        replyChannel.Reply ()
                        return! loop newData
                    | Query (url, replyChannel) ->
                        replyChannel.Reply (Map.tryFind url data)
                        return! loop data
                    | QueryKeys replyChannel ->
                        replyChannel.Reply (Seq.map (fun (k, _) -> Some k) (Map.toSeq data))
                        return! loop data
                    | Update (url, feedData, replyChannel) ->
                        let newData = Map.add url feedData data
                        replyChannel.Reply ()
                        return! loop newData
                    | QueryList (urls, replyChannel) ->
                        let toReply = seq {
                            for key in urls do
                                yield!
                                    match Option.bind(fun k -> Map.tryFind k data) key with
                                    | Some feedData -> [feedData]
                                    | None -> []                           
                        }
                        replyChannel.Reply toReply
                        return! loop data
                }
            loop Map.empty)

        interface IDataManager<URL option> with
            member this.Add feedData = 
                dataProcessor.PostAndAsyncReply(fun rc -> Add(feedData, rc) )
            member this.Remove key =
                match key with
                | Some k -> dataProcessor.PostAndAsyncReply(fun rc -> Remove(k, rc) )
                | _ -> async { return () }
            member this.Query key =
               match key with
                | Some k -> dataProcessor.PostAndAsyncReply(fun rc -> Query(k, rc) )
                | _ -> async { return None }
            member this.QueryKeys () =
                dataProcessor.PostAndAsyncReply QueryKeys
            member this.Update key feedData =
                match key with
                | Some k -> dataProcessor.PostAndAsyncReply(fun rc -> Update (k, feedData, rc) )
                | _ -> async { return () }
            member this.QueryList keys =
                dataProcessor.PostAndAsyncReply(fun rc -> QueryList(keys, rc))