module Pirates

open Elmish
open Elmish.React
open Elmish.Debug

let init() =

    Program.mkProgram State.init State.update View.root
    |> Program.withReact "elmish-app"
    #if DEBUG
    |> Program.withDebugger
    #endif
    |> Program.run

init()