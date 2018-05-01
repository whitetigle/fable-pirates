// ts2fable 0.6.0-build.320
module rec AnimeJs
open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

let [<Import("*","animejs")>] anime: Anime.IExports = jsNative

type [<AllowNullLiteral>] IExports =
    abstract anime: ``params``: Anime.AnimeParams -> Anime.AnimeInstance

type [<AllowNullLiteral>] FunctionBasedParamter =
    [<Emit "$0($1...)">] abstract Invoke: element: HTMLElement * index: float * length: float -> float

type [<AllowNullLiteral>] AnimeCallbackFunction =
    [<Emit "$0($1...)">] abstract Invoke: anim: Anime.AnimeInstance -> unit

type AnimeTarget =
    U5<string, obj, HTMLElement, SVGElement, NodeList> option

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AnimeTarget =
    let ofStringOption v: AnimeTarget = v |> Microsoft.FSharp.Core.Option.map U5.Case1
    let ofString v: AnimeTarget = v |> U5.Case1 |> Some
    let isString (v: AnimeTarget) = match v with None -> false | Some o -> match o with U5.Case1 _ -> true | _ -> false
    let asString (v: AnimeTarget) = match v with None -> None | Some o -> match o with U5.Case1 o -> Some o | _ -> None
    let ofObjOption v: AnimeTarget = v |> Microsoft.FSharp.Core.Option.map U5.Case2
    let ofObj v: AnimeTarget = v |> U5.Case2 |> Some
    let isObj (v: AnimeTarget) = match v with None -> false | Some o -> match o with U5.Case2 _ -> true | _ -> false
    let asObj (v: AnimeTarget) = match v with None -> None | Some o -> match o with U5.Case2 o -> Some o | _ -> None
    let ofHTMLElementOption v: AnimeTarget = v |> Microsoft.FSharp.Core.Option.map U5.Case3
    let ofHTMLElement v: AnimeTarget = v |> U5.Case3 |> Some
    let isHTMLElement (v: AnimeTarget) = match v with None -> false | Some o -> match o with U5.Case3 _ -> true | _ -> false
    let asHTMLElement (v: AnimeTarget) = match v with None -> None | Some o -> match o with U5.Case3 o -> Some o | _ -> None
    let ofSVGElementOption v: AnimeTarget = v |> Microsoft.FSharp.Core.Option.map U5.Case4
    let ofSVGElement v: AnimeTarget = v |> U5.Case4 |> Some
    let isSVGElement (v: AnimeTarget) = match v with None -> false | Some o -> match o with U5.Case4 _ -> true | _ -> false
    let asSVGElement (v: AnimeTarget) = match v with None -> None | Some o -> match o with U5.Case4 o -> Some o | _ -> None
    let ofNodeListOption v: AnimeTarget = v |> Microsoft.FSharp.Core.Option.map U5.Case5
    let ofNodeList v: AnimeTarget = v |> U5.Case5 |> Some
    let isNodeList (v: AnimeTarget) = match v with None -> false | Some o -> match o with U5.Case5 _ -> true | _ -> false
    let asNodeList (v: AnimeTarget) = match v with None -> None | Some o -> match o with U5.Case5 o -> Some o | _ -> None

module Anime =

    type [<AllowNullLiteral>] IExports =
        abstract speed: float
        abstract running: ResizeArray<AnimeInstance>
        abstract easings: obj
        abstract remove: targets: U2<AnimeTarget, ResizeArray<AnimeTarget>> -> unit
        abstract getValue: targets: AnimeTarget * prop: string -> U2<string, float>
        abstract path: path: U3<string, HTMLElement, SVGElement> option * ?percent: float -> (string -> obj)
        abstract setDashoffset: el: U2<HTMLElement, SVGElement> option -> float
        abstract bezier: x1: float * y1: float * x2: float * y2: float -> (float -> float)
        abstract timeline: ?``params``: U2<AnimeInstanceParams, ResizeArray<AnimeInstance>> -> AnimeTimelineInstance
        abstract random: min: float * max: float -> float

    type [<StringEnum>] [<RequireQualifiedAccess>] EasingOptions =
        | Linear
        | EaseInQuad
        | EaseInCubic
        | EaseInQuart
        | EaseInQuint
        | EaseInSine
        | EaseInExpo
        | EaseInCirc
        | EaseInBack
        | EaseInElastic
        | EaseOutQuad
        | EaseOutCubic
        | EaseOutQuart
        | EaseOutQuint
        | EaseOutSine
        | EaseOutExpo
        | EaseOutCirc
        | EaseOutBack
        | EaseOutElastic
        | EaseInOutQuad
        | EaseInOutCubic
        | EaseInOutQuart
        | EaseInOutQuint
        | EaseInOutSine
        | EaseInOutExpo
        | EaseInOutCirc
        | EaseInOutBack
        | EaseInOutElastic

    type [<StringEnum>] [<RequireQualifiedAccess>] DirectionOptions =
        | Reverse
        | Alternate
        | Normal

    type [<AllowNullLiteral>] AnimeInstanceParams =
        abstract loop: U2<float, bool> option with get, set
        abstract autoplay: bool option with get, set
        abstract direction: U2<DirectionOptions, string> option with get, set
        abstract ``begin``: AnimeCallbackFunction option with get, set
        abstract run: AnimeCallbackFunction option with get, set
        abstract update: AnimeCallbackFunction option with get, set
        abstract complete: AnimeCallbackFunction option with get, set

    type [<AllowNullLiteral>] AnimeAnimParams =
        abstract targets: U2<AnimeTarget, ReadonlyArray<AnimeTarget>> with get, set
        abstract duration: U2<float, FunctionBasedParamter> option with get, set
        abstract delay: U2<float, FunctionBasedParamter> option with get, set
        abstract elasticity: U2<float, FunctionBasedParamter> option with get, set
        abstract round: U3<float, bool, FunctionBasedParamter> option with get, set
        abstract easing: U3<EasingOptions, string, ReadonlyArray<float>> option with get, set
        abstract ``begin``: AnimeCallbackFunction option with get, set
        abstract run: AnimeCallbackFunction option with get, set
        abstract update: AnimeCallbackFunction option with get, set
        abstract complete: AnimeCallbackFunction option with get, set
        [<Emit "$0[$1]{{=$2}}">] abstract Item: AnyAnimatedProperty: string -> obj option with get, set

    type [<AllowNullLiteral>] AnimeParams =
        inherit AnimeInstanceParams
        inherit AnimeAnimParams

    type [<AllowNullLiteral>] AnimeInstance =
        abstract play: unit -> unit
        abstract pause: unit -> unit
        abstract restart: unit -> unit
        abstract reverse: unit -> unit
        abstract seek: time: float -> unit
        abstract began: bool with get, set
        abstract paused: bool with get, set
        abstract completed: bool with get, set
        abstract finished: Promise<unit> with get, set
        abstract ``begin``: AnimeCallbackFunction with get, set
        abstract run: AnimeCallbackFunction with get, set
        abstract update: AnimeCallbackFunction with get, set
        abstract complete: AnimeCallbackFunction with get, set
        abstract autoplay: bool with get, set
        abstract currentTime: float with get, set
        abstract delay: float with get, set
        abstract direction: string with get, set
        abstract duration: float with get, set
        abstract loop: U2<float, bool> with get, set
        abstract offset: float with get, set
        abstract progress: float with get, set
        abstract remaining: float with get, set
        abstract reversed: bool with get, set
        abstract animatables: ReadonlyArray<obj> with get, set
        abstract animations: ReadonlyArray<obj> with get, set

    type [<AllowNullLiteral>] AnimeTimelineAnimParams =
        inherit AnimeAnimParams
        abstract offset: U3<float, string, FunctionBasedParamter> with get, set

    type [<AllowNullLiteral>] AnimeTimelineInstance =
        inherit AnimeInstance
        abstract add: ``params``: AnimeAnimParams -> AnimeTimelineInstance
