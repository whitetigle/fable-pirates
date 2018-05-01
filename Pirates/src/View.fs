module View 

open Types
open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma

let root (model:Model) dispatch= 
  let knobs = 
    model.Knobs
    |> Seq.map( fun rows -> 
      let columns = 
        rows
        |> Seq.map( fun knob ->
          Column.column [] [
            yield (
              match knob.Status with 
              | KnobStatus.Hidden -> Notification.notification [ Notification.Color IsDark ] [ str (string knob.PreviousValue) ]  
              | Visible -> Notification.notification [ Notification.Color IsSuccess ] [ str (string knob.PreviousValue) ]  
            )
          ]
        )
        |> Seq.toList
      
      Columns.columns [] columns
    )
    |> Seq.toList

  Hero.hero [] [
    Hero.body [] knobs
  ]
    
  