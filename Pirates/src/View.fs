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

  match model.EndOfGame with 
  | Some ending -> 

    let title, textColor, backColor = 
      match ending with 
      | Lost -> "Ah la la ! Vous avez perdu !", "is-dark", IsDanger
      | Won -> "Bravo !", "is-dark", IsSuccess

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
  
  | None -> 
    let card = 
      match model.ActiveCard with 
      | Some card ->
        let label = 
          match card.Item with 
          | Card label -> label
          | Repetor label -> label 
          | Nothing -> ""
          | _ -> printfn "%A" card.Item; ""

        div[ ClassName "label is-dark"] [         
          label |> Inside.Str
        ]

      | None -> str "" 

    // TODO handle Midi event to hide notification
    let notification model= 
      let title, isActive= 
        match model.NotificationMessage with 
        | Some msg -> 
          let title, m = 
            match msg.Title with 
            | GameOver msg ->  "Game Over", msg
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

    Hero.hero [Hero.Color IsLight; Hero.IsFullHeight] [
      notification model
      Container.container 
        [Container.IsFluid; Container.Modifiers [Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]] 
        [ card ]
        => Inside.Hero.Body
    ]
      
    