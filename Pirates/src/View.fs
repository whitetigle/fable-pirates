module View 

open Types
open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.Extensions

module Inside = GoGoFulma.Inside
open GoGoFulma.ReactHelpers

let root (model:Model) dispatch= 

  let card = 
    match model.ActiveCard with 
    | Some card ->
      let label = 
        match card.Item with 
        | Card label -> label
        | Trap label -> label 
        | Nothing -> ""

      label |> Inside.Str => Inside.Heading.h1  
    | None -> str "" 


  // TODO handle Midi event to hide notification
  let notification = 
    let active =  model.NotificationMessage.Title.IsSome
    let hide _ = HideNotificationMessage |> dispatch
    let title, message =
      if active then  
        match model.NotificationMessage.Title.Value with 
        | GameOver msg -> "GameOver", msg
      else "",""
    Modal.modal [ Modal.IsActive active; Modal.Props [OnClick hide] ]
      [
         Modal.background [ Props [ OnClick hide ] ] [ ]
         Notification.notification [ Notification.Color IsInfo]
           [
             Heading.h1 [] [ str title]
             p [] [ str message ]
           ]
      ]

  Hero.hero [] [
    notification
    card => Inside.Hero.Body
  ]
    
  