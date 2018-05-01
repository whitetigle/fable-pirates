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
          Column.column [] [str (string knob.Status)]
        )
        |> Seq.toList
      
      Columns.columns [] columns
    )
    |> Seq.toList

  Hero.hero [] 
    knobs
  