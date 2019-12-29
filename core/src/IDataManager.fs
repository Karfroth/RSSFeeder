module IDataManager
    type IDataManager<'key> =
        abstract member Add: FeedModel.FeedData -> 'key
        abstract member Remove: 'key -> unit
        abstract member Query: 'key -> FeedModel.FeedData
        abstract member QueryList: 'key seq -> FeedModel.FeedData seq