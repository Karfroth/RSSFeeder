module IDataManager
    type IDataManager<'key> =
        abstract member Add: FeedModel.FeedData -> Async<'key option>
        abstract member Remove: 'key -> Async<unit>
        abstract member Query: 'key -> Async<FeedModel.FeedData option>
        abstract member QueryAll: unit -> Async<FeedModel.FeedData seq>
        abstract member Update: 'key -> FeedModel.FeedData -> Async<unit>
        abstract member QueryList: 'key seq -> Async<FeedModel.FeedData seq>