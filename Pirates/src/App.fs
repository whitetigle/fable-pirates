module Pirates

open Elmish
open Elmish.React


let init() =

    Program.mkProgram State.init State.update View.root
    |> Program.withReact"elmish-app"
    |> Program.run

init()