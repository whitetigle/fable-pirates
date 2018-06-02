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

  match model.Step with 
  | StartGame -> 

    let title, textColor, backColor = 
      "Démarrer", "is-dark", IsSuccess

    let card = 
      div[ ClassName (sprintf "label %s" textColor)] [         
        title |> Inside.Str
      ]

    Hero.hero [Hero.Color backColor; Hero.IsFullHeight] [
      Container.container 
        [
          Container.IsFluid
          Container.Modifiers [Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]
          Container.Props[ OnClick (fun _ -> Msg.StartGame |> dispatch ) ]
        ] 
        [ card ]
        => Inside.Hero.Body
    ]

  | Won -> 

    let title, textColor, backColor = 
      "Bravo !", "is-dark", IsSuccess

    let card = 
      div[ ClassName (sprintf "label %s" textColor)] [         
        title |> Inside.Str
      ]

    Hero.hero [Hero.Color backColor; Hero.IsFullHeight] [
      Container.container 
        [
          Container.IsFluid
          Container.Modifiers [Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]
          Container.Props[ OnClick (fun _ -> ResetGame |> dispatch ) ]
        ] 
        [ card ]
        => Inside.Hero.Body
    ]
  
  | GameStarted -> 
    let card = 
      match model.ActiveCard with 
      | Some card ->
        let label, color = 
          match card.Item with 
          | Card label -> label, "has-text-dark"
          | Mixator label -> label, "has-text-danger" 
          | Knobator label -> label, "has-text-danger" 
          | Nothing -> "", "has-text-dark"

        div[ ClassName (sprintf "label %s" color)] [         
          label |> Inside.Str
        ]

      | None -> str "" 

    // TODO handle Midi event to hide notification
    let notification model= 
      let title, isActive= 
        match model.NotificationMessage with 
        | Some msg -> 
          let title, m = 
            match msg with 
            | GoodMove msg -> "Bien joué !", msg
            | BadMove msg -> "Zut !", msg
          [
            Heading.h1 [] [ str title]
            p [] [ str m ]
          ]
          , true
        | None -> [str ""], false

      let hide _ = HideNotificationMessage |> dispatch

      Modal.modal [ Modal.IsActive isActive; Modal.Props [OnClick hide] ]
        [
           Modal.background [ Props [ OnClick hide ] ] [ ]
           Notification.notification [ Notification.Color IsInfo]
             title
        ]

    let color = 
      match model.GoodMove with 
      | None -> IsLight
      | Some Bad -> IsDanger 
      | Some Good -> IsSuccess
      | Some Trap -> IsDark

    Hero.hero [Hero.Color color; Hero.IsFullHeight] [
      notification model
      Container.container 
        [Container.IsFluid; Container.Modifiers [Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]] 
        [ card ]
        => Inside.Hero.Body
    ]
      
    