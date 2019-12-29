module IFeedManager    

    [<AbstractClass>]
    type IFeedManager<'key, 'source, 'data>(dispatcher: 'data -> unit) =  // TODO: Replace dispatcher type by correct one
        abstract member Add: 'source -> unit
        abstract member Remove: 'key -> unit
        abstract member Update: 'key -> unit
        abstract member UpdateAll: unit -> unit