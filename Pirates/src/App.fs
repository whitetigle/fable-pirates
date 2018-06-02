module Pirates

open Elmish
open Elmish.React
open Elmish.Debug
open Fable.Import.Howler
open Fable.Core.JsInterop
open Types

let init() =

    let createSound name = 
        let name = sprintf "sounds/%s" name
        let props = jsOptions<IHowlProperties>(fun opt -> 
            opt.src <- !![|name|]
        )
        howler.Howl.Create( props)

    let createLoop name = 
        let name = sprintf "sounds/%s" name
        let props = jsOptions<IHowlProperties>(fun opt -> 
            opt.src <- !![|name|]
            opt.loop <- Some true
        )
        howler.Howl.Create( props)

    let sounds =
        {
          Knob=createSound "Knob.ogg"
          Good=createSound "Good.ogg"
          Bad=createSound "Bad.ogg"
          Trap=createSound "Trap.ogg"
          GameTrack=createLoop "GameTrack.ogg"
          MenuTrack=createLoop "MenuTrack.ogg"
          Win=createSound "Win.ogg"
        }
    
    Program.mkProgram (State.init sounds) State.update View.root
    |> Program.withReact "elmish-app"
    |> Program.run

init()