module State
open Elmish
open Types

let init _ = 
  let bModel,bCmd = Behringer.init()

  Model.Create bModel,
  Cmd.batch [ Cmd.map BehringerMsg bCmd]

let update (msg: Msg) (model: Model) =

  match msg with 
  | BehringerMsg subMsg ->
    model, Cmd.none
  | _ -> 
    model,Cmd.none