module CoreActorTest

    open Expecto

    type TestDataManager () =
        interface IDataManager.IDataManager<FeedModel.URL> with

            member this.Add data = async { return data.url }
            member this.Remove key = async { return () }
            member this.Query key = async { return None }
            member this.QueryKeys () = async { return Seq.empty }
            member this.Update key data = async { return () }
            member this.QueryList keys = async { return Seq.empty }

    let testDataManager = TestDataManager ()