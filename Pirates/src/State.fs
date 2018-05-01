module State
open Elmish
open Types
open Fable.Import

[<Literal>]
let KNOBSTARTINDEX = 80

[<Literal>]
let KNOB_THRESHOLD = 10

let init _ = 
  let bModel,bCmd = Behringer.init()

  Model.Create bModel,
  Cmd.batch [ Cmd.map BehringerMsg bCmd]

let update (msg: Msg) (model: Model) =

  match msg with 
  | BehringerMsg bMsg ->
      match bMsg with 
      | Behringer.OnKnob (index, value) -> 
        let realIndex = index - KNOBSTARTINDEX
        let width = MAXX + 1

        // WTF is going on with these indexes?
        let x,y = 
          let y = realIndex % width
          let y = if y = 0 then MAXX else y - 1          
          let x = realIndex / width
          let x = if y = MAXX then x - 1 else x
          x,y

        let knob = model.Knobs.[x].[y]
        let updatedKnob = 

          let turnLeft knob = 
            let knob = {knob with TurnRightCounter=0}
            match knob.TurnLeftCounter with 
            | times when times > KNOB_THRESHOLD ->
              {knob with Status=Hidden}
            | _ -> 
              {knob with TurnLeftCounter=knob.TurnLeftCounter+1}

          let turnRight knob = 
            let knob = {knob with TurnLeftCounter=0}
            match knob.TurnRightCounter with 
            | times when times > KNOB_THRESHOLD ->
              {knob with Status=Visible}
            | _ -> 
              {knob with TurnRightCounter=knob.TurnRightCounter+1}

          let updated = 
            match knob.PreviousValue,value with 
            | _, 0 -> turnLeft knob

            | _, 127 -> turnRight knob

            |  prev, newOne when newOne < prev -> turnLeft knob            
            
            | prev, newOne when newOne > prev -> turnRight knob

            | _ -> knob
          
          { updated with PreviousValue=value}
 
        let updatedKnobs = 
          model.Knobs
          |> Array.mapi( fun i knobs -> 
                knobs
                |> Array.mapi( fun j knob -> 
                  if i=x && j=y then 
                    updatedKnob
                  else
                    knob
                )
          )
        let updatedModel = 
          { model
            with Knobs=updatedKnobs
          }

        updatedModel, Cmd.none

      | _ -> 
        let (m, c) = Behringer.update bMsg model.Behringer
        { model with Behringer = m },
        Cmd.map BehringerMsg c

  | _ -> 
    model,Cmd.none