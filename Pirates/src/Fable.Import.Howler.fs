// ts2fable 0.6.0-build.320
module rec Fable.Import.Howler
open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

let [<Import("*","howler")>] howler: Howler.IExports = jsNative
let [<Import("*","howler")>] Howler: HowlerGlobal = jsNative

type [<AllowNullLiteral>] IExports =
    abstract Howl: HowlStaticStatic

type [<AllowNullLiteral>] HowlerGlobal =
    abstract mute: muted: bool -> unit
    abstract volume: unit -> float
    abstract volume: volume: float -> HowlerGlobal
    abstract codecs: ext: string -> bool
    abstract unload: unit -> unit
    abstract usingWebAudio: bool with get, set
    abstract noAudio: bool with get, set
    abstract mobileAutoEnable: bool with get, set
    abstract autoSuspend: bool with get, set
    abstract ctx: AudioContext with get, set
    abstract masterGain: GainNode with get, set
    abstract stereo: pan: float -> HowlerGlobal
    abstract pos: x: float * y: float * z: float -> U2<HowlerGlobal, unit>
    abstract orientation: x: float * y: float * z: float * xUp: float * yUp: float * zUp: float -> U2<HowlerGlobal, unit>

type [<AllowNullLiteral>] IHowlSoundSpriteDefinition =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: name: string -> U2<float * float, float * float * bool> with get, set

type [<AllowNullLiteral>] IHowlProperties =
    abstract src: U2<string, ResizeArray<string>> with get, set
    abstract volume: float option with get, set
    abstract html5: bool option with get, set
    abstract loop: bool option with get, set
    abstract preload: bool option with get, set
    abstract autoplay: bool option with get, set
    abstract mute: bool option with get, set
    abstract sprite: IHowlSoundSpriteDefinition option with get, set
    abstract rate: float option with get, set
    abstract pool: float option with get, set
    abstract format: U2<ResizeArray<string>, string> option with get, set
    abstract xhrWithCredentials: bool option with get, set
    abstract onload: (unit -> unit) option with get, set
    abstract onloaderror: (float -> obj option -> unit) option with get, set
    abstract onplay: (float -> unit) option with get, set
    abstract onplayerror: (float -> obj option -> unit) option with get, set
    abstract onend: (float -> unit) option with get, set
    abstract onpause: (float -> unit) option with get, set
    abstract onstop: (float -> unit) option with get, set
    abstract onmute: (float -> unit) option with get, set
    abstract onvolume: (float -> unit) option with get, set
    abstract onrate: (float -> unit) option with get, set
    abstract onseek: (float -> unit) option with get, set
    abstract onfade: (float -> unit) option with get, set

type [<AllowNullLiteral>] Howl =
    abstract play: ?spriteOrId: U2<string, float> -> float
    abstract pause: ?id: float -> Howl
    abstract stop: ?id: float -> Howl
    abstract mute: unit -> bool
    abstract mute: muted: bool * ?id: float -> Howl
    abstract volume: unit -> float
    abstract volume: idOrSetVolume: float -> U2<Howl, float>
    abstract volume: volume: float * id: float -> Howl
    abstract fade: from: float * ``to``: float * duration: float * ?id: float -> Howl
    abstract rate: unit -> float
    abstract rate: idOrSetRate: float -> U2<Howl, float>
    abstract rate: rate: float * id: float -> Howl
    abstract seek: ?seek: float * ?id: float -> U2<Howl, float>
    abstract loop: ?loop: bool * ?id: float -> Howl
    abstract playing: ?id: float -> bool
    abstract duration: ?id: float -> float
    [<Emit "$0.on('load',$1,$2)">] abstract on_load: callback: (unit -> unit) * ?id: float -> Howl
    [<Emit "$0.on('loaderror',$1,$2)">] abstract on_loaderror: callback: (float -> obj option -> unit) * ?id: float -> Howl
    [<Emit "$0.on('play',$1,$2)">] abstract on_play: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.on('playerror',$1,$2)">] abstract on_playerror: callback: (float -> obj option -> unit) * ?id: float -> Howl
    [<Emit "$0.on('end',$1,$2)">] abstract on_end: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.on('pause',$1,$2)">] abstract on_pause: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.on('stop',$1,$2)">] abstract on_stop: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.on('mute',$1,$2)">] abstract on_mute: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.on('volume',$1,$2)">] abstract on_volume: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.on('rate',$1,$2)">] abstract on_rate: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.on('seek',$1,$2)">] abstract on_seek: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.on('fade',$1,$2)">] abstract on_fade: callback: (float -> unit) * ?id: float -> Howl
    abstract on: ``event``: string * callback: Function * ?id: float -> Howl
    [<Emit "$0.once('load',$1,$2)">] abstract once_load: callback: (unit -> unit) * ?id: float -> Howl
    [<Emit "$0.once('loaderror',$1,$2)">] abstract once_loaderror: callback: (float -> obj option -> unit) * ?id: float -> Howl
    [<Emit "$0.once('play',$1,$2)">] abstract once_play: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.once('playerror',$1,$2)">] abstract once_playerror: callback: (float -> obj option -> unit) * ?id: float -> Howl
    [<Emit "$0.once('end',$1,$2)">] abstract once_end: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.once('pause',$1,$2)">] abstract once_pause: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.once('stop',$1,$2)">] abstract once_stop: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.once('mute',$1,$2)">] abstract once_mute: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.once('volume',$1,$2)">] abstract once_volume: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.once('rate',$1,$2)">] abstract once_rate: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.once('seek',$1,$2)">] abstract once_seek: callback: (float -> unit) * ?id: float -> Howl
    [<Emit "$0.once('fade',$1,$2)">] abstract once_fade: callback: (float -> unit) * ?id: float -> Howl
    abstract once: ``event``: string * callback: Function * ?id: float -> Howl
    abstract off: ``event``: string * ?callback: Function * ?id: float -> Howl
    abstract state: unit -> U3<string, string, string>
    abstract load: unit -> unit
    abstract unload: unit -> unit
    abstract stereo: pan: float * ?id: float -> U2<Howl, unit>
    abstract pos: x: float * y: float * z: float * ?id: float -> U2<Howl, unit>
    abstract orientation: x: float * y: float * z: float * xUp: float * yUp: float * zUp: float -> U2<Howl, unit>
    abstract pannerAttr: o: HowlPannerAttrO * ?id: float -> Howl

type [<AllowNullLiteral>] HowlPannerAttrO =
    abstract coneInnerAngle: float option with get, set
    abstract coneOuterAngle: float option with get, set
    abstract coneOuterGain: float option with get, set
    abstract distanceModel: U2<string, string> with get, set
    abstract maxDistance: float with get, set
    abstract panningModel: U2<string, string> with get, set
    abstract refDistance: float with get, set
    abstract rolloffFactor: float with get, set

type [<AllowNullLiteral>] HowlStaticStatic =
    [<Emit "new $0($1...)">] abstract Create: properties: IHowlProperties -> Howl

module Howler =

    type [<AllowNullLiteral>] IExports =
        abstract Howler: HowlerGlobal
        abstract Howl: HowlStaticStatic
