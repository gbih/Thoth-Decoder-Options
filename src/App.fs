module ThothDecoderExamples

open Thoth.Json
open Fable.Core


module Print =

    let elementId = "elmish-app"
    let elem = Browser.Dom.document.getElementById(elementId)
    elem.setAttribute("style", "color:black; margin:1rem; display: block;font-family: monospace;font-size:1.1rem;white-space: pre-wrap;"; )
    
    let p input =
        let x = input
        let showElement = Browser.Dom.document.createElement("span")
        showElement.innerHTML <- sprintf "%A\n" x
        Browser.Dom.document.getElementById(elementId).appendChild showElement |> ignore

    let title input =
        let x = input
        let showElement = Browser.Dom.document.createElement("span")
        showElement.setAttribute("style", "margin-bottom:1rem;font-style:italic;color:blue"; )
        showElement.innerHTML <- sprintf "%A\n" x
        Browser.Dom.document.getElementById(elementId).appendChild showElement |> ignore
        

let log = Print.p
let logtitle = Print.title

let HR () = "- - - - - - - - - - - - - - - - - - -" |> log
let SECTION() = "= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =" |> log 


let json =
    """
{
    "total": 94969,
    "total_pages": 31657,
    "results": [
        {
            "id": "GRV4ypBKgxE",
            "color": null,
            "description": "",
            "alt_description": "orange Audi coupe parked on gray concrete road",
            "urls": {
                "raw": "https://images.unsplash.com/photo-1504215680853-026ed2a45def?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjg3NTAzfQ",
                "full": "https://images.unsplash.com/photo-1504215680853-026ed2a45def?ixlib=rb-1.2.1&q=85&fm=jpg&crop=entropy&cs=srgb&ixid=eyJhcHBfaWQiOjg3NTAzfQ",
                "regular": "https://images.unsplash.com/photo-1504215680853-026ed2a45def?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjg3NTAzfQ"
            }
        },
        {
            "id": "lrmo2hlFYE4",
            "color": "#F5F3F2",
            "description": null,
            "alt_description": null,
            "urls": {
                "raw": "https://images.unsplash.com/photo-1523676060187-f55189a71f5e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjg3NTAzfQ",
                "full": "https://images.unsplash.com/photo-1523676060187-f55189a71f5e?ixlib=rb-1.2.1&q=85&fm=jpg&crop=entropy&cs=srgb&ixid=eyJhcHBfaWQiOjg3NTAzfQ",
                "regular": "https://images.unsplash.com/photo-1523676060187-f55189a71f5e?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjg3NTAzfQ",
                "small": "",
                "thumb": "https://images.unsplash.com/photo-1523676060187-f55189a71f5e?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=200&fit=max&ixid=eyJhcHBfaWQiOjg3NTAzfQ"
            }
        },
        {
            "id": "xpcUYaZtplI",
            "color": "#EB072C",
            "description": null,
            "alt_description": "red Audi coupe on road near trees at daytime",
            "urls": {}
        }
    ]
}
    """.Trim()


type UrlsChild =
    {
        raw : string option
        full : string option
        regular : string option
        small : string option
        thumb : string option
        superlarge: string option
    }
    static member Decoder =
        Decode.object (fun get ->
            {
                raw = get.Optional.Field "raw" Decode.string 
                full = get.Optional.Field "full" Decode.string
                regular = get.Optional.Field "regular" Decode.string
                small = get.Optional.Field "small" Decode.string
                thumb = get.Optional.Field "thumb" Decode.string
                superlarge = get.Optional.Field "superlarge" Decode.string
            }
        )

type Child = 
    {
        id : string
        color : string option
        description : string option
        alt_description : string option
        urls : UrlsChild option
    }
    static member Decoder =
        Decode.object (fun get ->                
            { 
                id = get.Required.Field "id" Decode.string
                color = get.Optional.Field "color" Decode.string
                description = get.Optional.Field "description" Decode.string
                alt_description = get.Optional.Field "alt_description" Decode.string
                urls = get.Optional.Field "urls" UrlsChild.Decoder
            }
        )

type Results =
    {
        results: Child array
    }
    static member Decoder = Decode.list Child.Decoder


// -- Auto decode with defined types
let AutoDecodeWithOptions ()=
    Decode.Auto.fromString<Results>(json, isCamelCase=true)

"AutoDecodeWithOptions" |> logtitle
AutoDecodeWithOptions() |> log
AutoDecodeWithOptions() |> printfn "AutoDecodeWithOptions: \n%A"


HR()


// -- Manually decode with defined types and Decoders
let ManualDecodeWithOptions ()=
    let resultDecoder = Decode.field "results" Results.Decoder
    Decode.fromString resultDecoder json

"ManualDecodeWithOptions" |> logtitle
ManualDecodeWithOptions() |> log
ManualDecodeWithOptions() |> printfn "ManualDecodeWithOptions: \n%A"

