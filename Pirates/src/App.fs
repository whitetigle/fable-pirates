module Pirates

open Elmish
open Elmish.React
open Elmish.Debug
open Fable.Import.Howler
open Fable.Core.JsInterop
open Types

let init() =

    let sounds =
        let props = jsOptions<IHowlProperties>(fun opt -> 
            opt.src <- !![|"sounds/knob.ogg"|]
        )
        let knob = howler.Howl.Create( props)
        {
          Knob=knob
        }
    
    
    Program.mkProgram (State.init sounds) State.update View.root
    |> Program.withReact "elmish-app"
    |> Program.run

init()