module SQLiteStorage

open System
open SQLite
open IDataManager
open FeedModel
open Thoth.Json.Net
// title: string; summary: string; authors: string seq; date: string; links: URL seq
type FeedItem with
    static member Decoder: Decoder<FeedItem> =
        Thoth.Json.Net.Decode.object
            (fun get -> 
                let stringListDecoder = Decode.list Decode.string
                let links = Seq.map URL (get.Required.Field "links" stringListDecoder)
                {
                    title = get.Required.Field "title" Decode.string
                    summary = get.Required.Field "summary" Decode.string
                    authors = get.Required.Field "authors" stringListDecoder
                    date = get.Required.Field "date" Decode.string
                    links = links
                }
            )

let encodeFeedItem (feedItem: FeedItem) =
    Encode.object
        [ "title", Encode.string feedItem.title
          "summary", Encode.string feedItem.summary
          "authors", Seq.map Encode.string feedItem.authors |> Seq.toList |> Encode.list
          "date", Encode.string feedItem.date
          "links", Seq.map (fun (URL url) -> Encode.string url) feedItem.links |> Seq.toList |> Encode.list
        ]

type FeedDataEntity() =
    [<PrimaryKey; AutoIncrement>]
    member val Id: int = 0 with get, set
    [<Unique>]
    member val Url: string = "" with get, set
    member val LastSyncTime: int64 = 0L with get, set
    member val FeedName: string = "" with get, set
    member val Articles: string = "" with get, set

let convertFromEntity (entity: FeedDataEntity): FeedData option =
    match Decode.fromString (Decode.list FeedItem.Decoder) entity.Articles with
    | Ok articles -> 
        Some {
            lastSyncTime = entity.LastSyncTime
            feedName = entity.FeedName
            articles = articles
            url = URL entity.Url
            id = Some entity.Id
        }
    | _ -> None

let convertToEntity (data: FeedData) =
    let entity = FeedDataEntity()
    let (URL urlString) = data.url
    let articlesJson = Seq.map encodeFeedItem data.articles |> Seq.toList
    entity.Url <- urlString
    entity.FeedName <- data.feedName
    entity.LastSyncTime <- data.lastSyncTime
    entity.Articles <- Encode.list articlesJson |> Encode.toString 0
    match data.id with
    | Some id ->
        entity.Id <- id
        entity
    | None -> entity

type SQLiteDataManager(dbPath: string) =
    let connect () = async {
        let db = SQLiteAsyncConnection(SQLiteConnectionString dbPath)
        do! db.CreateTableAsync<FeedDataEntity>() |> Async.AwaitTask |> Async.Ignore
        return db
    }

    interface IDataManager<int> with
        member this.Add (data: FeedModel.FeedData) = async {
            let! db = connect()
            let entity = convertToEntity data
            do! db.InsertAsync(entity) |> Async.AwaitTask |> Async.Ignore
            let! rowIdObj = db.ExecuteScalarAsync("select last_insert_rowid()", [||]) |> Async.AwaitTask
            return rowIdObj |> int |> Some
        }
        member this.Remove (id: int) = 
            async {
                let! db = connect ()
                do! db.DeleteAsync(id) |> Async.AwaitTask |> Async.Ignore
                return ()
            }
        member this.Query (id: int) = 
           async {
               let! db = connect ()
               let! entity =  db.FindAsync<FeedDataEntity>(id) |> Async.AwaitTask
               return convertFromEntity entity
           }
        member this.QueryAll () =
            async {
                let! db = connect ()
                let! items = db.Table<FeedDataEntity>().ToArrayAsync() |> Async.AwaitTask
                return seq {
                    for entityData in items do
                        yield!
                            match convertFromEntity entityData with
                            | Some data -> seq { data }
                            | None -> Seq.empty
                }
            }
        member this.Update id feedData =
            let entity = convertToEntity feedData
            if entity.Id <> 0
            then 
                async {
                    let! db = connect()
                    
                    do! db.UpdateAsync entity |> Async.AwaitTask |> Async.Ignore
                    return ()
                }
            else async { return () }
        member this.QueryList keys =
            async {
                let! db = connect ()
                // TODO: Don't do this way
                let! allItems = db.Table<FeedDataEntity>().ToArrayAsync() |> Async.AwaitTask
                return seq {
                    for entity in allItems do
                        let convertResult = if Seq.contains entity.Id keys then convertFromEntity entity else None
                        match convertResult with
                        | Some value -> yield value
                        | None -> ()
                }
            }