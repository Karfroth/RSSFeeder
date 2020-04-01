module RssFeeder.Bolero.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client

/// Routing endpoints definition.

type FeedData = {title: string; url: string}

type Message =
    | Add of string
    | UpdateURL of string

type Model = {
    feeds: FeedData seq
    urlInput: string
}

let initModel = {feeds = Seq.empty; urlInput = ""}

let update message model =
    match message with
    | Add url ->
        printfn "Add URL %s" url 
        { 
            model with
                feeds = Seq.append model.feeds [{title = String.concat "-" ["Title"; url]; url = url}];
                urlInput = ""
        }
    | UpdateURL updatedURL-> { model with urlInput = updatedURL }    

let view model dispatch =
    div [] [
        input [
            attr.value model.urlInput
            on.change (fun e -> (dispatch << UpdateURL << unbox) e.Value)
        ]
        button [
            on.click (fun _ -> 
                printfn "urlInput is %s" model.urlInput
                dispatch (Add model.urlInput)
            )
        ] [text "add"]
        forEach model.feeds (fun m -> 
            text m.title
        )
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
#if DEBUG
        |> Program.withHotReload
#endif
