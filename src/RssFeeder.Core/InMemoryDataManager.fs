module InMemoryDataManager
    open IDataManager
    open FeedModel
    open Shielded

    type InMemoryDataManager () =

        let shielded = Shielded<Map<URL, FeedData>>()

        interface IDataManager<URL option> with
            member this.Add feedData = 
                async {
                    let oldMap = shielded.Value;
                    Shield.InTransaction(System.Action(fun _ ->
                        match feedData.url with
                        | Some url -> shielded.Value <- Map.add url feedData oldMap
                        | _ -> ()
                    ))
                    return feedData.url
                }
            member this.Remove key =
                match key with
                | Some k ->
                    async {
                        let oldMap = shielded.Value;
                        Shield.InTransaction(System.Action(fun _ ->
                            shielded.Value <- Map.remove k oldMap
                        ))
                    }
                | None -> async { return () }
            member this.Query key =
                match key with
                | Some k -> async { return Map.tryFind k shielded.Value }
                | None -> async { return None }
            member this.QueryKeys () =
                async { return Seq.map (fun (k, _) -> Some k) (Map.toSeq shielded.Value) }
            member this.Update key feedData =
                match key with
                | Some k ->
                    async {
                        let oldMap = shielded.Value;
                        Shield.InTransaction(System.Action(fun _ ->
                            match feedData.url with
                            | Some url -> shielded.Value <- Map.add url feedData oldMap
                            | _ -> ()
                        ))
                    }
                | None -> async { return () }
            member this.QueryList keys = 
                async {
                    return seq {
                        for key in keys do
                            yield!
                                match Option.bind(fun k -> Map.tryFind k shielded.Value) key with
                                | Some feedData -> [feedData]
                                | None -> []                           
                    }
                }