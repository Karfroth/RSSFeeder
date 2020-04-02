module IFeedManager    

    [<AbstractClass>]
    type IFeedManager<'key, 'source, 'data>() =  // TODO: Replace dispatcher type by correct one
        abstract member Add: ('data -> unit) -> 'source -> unit
        abstract member Remove: ('data -> unit) -> 'key -> unit
        abstract member Update: ('data -> unit) -> 'key -> unit
        abstract member UpdateAll: ('data -> unit) -> unit -> unit