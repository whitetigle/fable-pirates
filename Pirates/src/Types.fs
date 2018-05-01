module Types

open Fable.Core

[<Literal>]
let MAXY = 2

[<Literal>]
let MAXX = 7

type RenderMsg = Render
type Msg= 
  | BehringerMsg of Behringer.Msg

[<StringEnum>]
type KnobStatus = Visible | Hidden

type Knob = 
  {
    Status:KnobStatus
    PreviousValue:int
    TurnLeftCounter:int
    TurnRightCounter:int
  }
  static member Start() = 
    {
      Status = Hidden
      PreviousValue=0
      TurnLeftCounter=0
      TurnRightCounter=0
    }

type Model = 
  { 
    Knobs:Knob array array
    Behringer:Behringer.Model 
  }
  static member Create (bModel:Behringer.Model) = 
    let knobs = 
        [| 0..MAXY |] 
        |> Array.map( fun _ -> 
            [| 0..MAXX |] |> Array.map( fun _ -> Knob.Start())
          )
    {
      Knobs = knobs
      Behringer= bModel
    }