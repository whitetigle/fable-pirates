module Pirates

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Elmish
open Elmish.React


let init() =

    Program.mkProgram State.init State.update View.root
    |> Program.withReactUnoptimized "elmish-app"
    |> Program.run

init()