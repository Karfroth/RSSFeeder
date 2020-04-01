module InMemoryDataManager
    open IDataManager
    open FeedModel
    open Akka.FSharp

    type InMemoryDataHanlderMsg = 
        | Add of FeedData
        | Remove of URL
        | Query of URL
        | QueryKeys
        | Update of URL * FeedData
        | QueryList of URL option seq

    let handleInMemoryData (mailbox: Actor<InMemoryDataHanlderMsg>) =
        let rec loop(data: Map<URL, FeedData>) = actor {
            let sender = mailbox.Sender ()
            match! mailbox.Receive() with
            | Add feedData ->
                let newData = 
                    match feedData.url with
                    | Some url -> Map.add url feedData data
                    | None -> data
                return! loop newData
            | Remove key ->
                return! loop (Map.remove key data)
            | Query key ->
                sender <! Map.tryFind key data
                return! loop data
            | QueryKeys ->
                let keys = Seq.map (fun (k, _) -> k) (Map.toSeq data)
                sender <! keys
                return! loop data
            | Update (url, feedData) ->
                let newData = Map.add url feedData data
                return! loop newData
            | QueryList optionalKeys ->
                sender <! seq { 
                    for optionalKey in optionalKeys do 
                        yield! match Option.bind (fun k -> Map.tryFind k data) optionalKey with
                               | Some feedData -> seq { feedData }
                               | None -> Seq.empty
                }
                return! loop data
        }
        loop(Map.empty)

    type InMemoryDataManager<'a> (parentActor: Actor<'a>, actorIDOpt: string option) =

        let actorID =
            match actorIDOpt with
            | Some id -> id
            | None -> System.Random().Next().ToString()
        let inMemoryDataActor = spawn parentActor actorID handleInMemoryData

        interface IDataManager<URL option> with
            member this.Add feedData = inMemoryDataActor <? (Add feedData)
            member this.Remove key =
                match key with
                | Some k -> inMemoryDataActor <? (Remove k)
                | None -> async { return () }
            member this.Query key =
                match key with
                | Some k -> inMemoryDataActor <? (Query k)
                | None -> async { return None }
            member this.QueryKeys () = inMemoryDataActor <? QueryKeys
            member this.Update key feedData =
                match key with
                | Some k -> inMemoryDataActor <? (Update (k, feedData))
                | None -> async { return () }
            member this.QueryList keys = inMemoryDataActor <? (QueryList keys)