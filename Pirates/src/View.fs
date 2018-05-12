module View 

open Types
open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma

let root (model:Model) dispatch= 
  let knobs = 
    model.Deck
    |> Seq.map( fun rows -> 
      let columns = 
        rows
        |> Seq.map( fun knob ->
          Column.column [ Column.Props[ OnClick ( fun _ -> (ToggleCard knob.Pos) |> dispatch)] ] [
            yield (
              let label = 
                match knob.Item with 
                | Some item -> (string item)
                | None -> ""
              match knob.Status with 
              | CardStatus.Disabled -> 
                match knob.Item with 
                | Some item -> 
                  match item with 
                  | Nothing -> 
                    Notification.notification [ Notification.Color IsLight ] 
                      [ str "Empty Slot" ]  
                  | _ -> 
                    Notification.notification [ Notification.Color IsDark ] 
                      [ str label ]  
  //                  [ str (string knob.PreviousValue) ]
                | None -> failwith "not handled"    
              | Activated -> 
                Notification.notification [ Notification.Color IsSuccess ] 
                  [ str label ]  
//                  [ str (string knob.PreviousValue) ]  
            )
          ]
        )
        |> Seq.toList
      
      Columns.columns [] columns
    )
    |> Seq.toList

  let header = 

    let stat value name= 
      Column.column [] [
        Notification.notification [Notification.Color IsDark] 
          [ 
            Heading.h2 [] [ str name] 
            p [] [ str <| sprintf "%i" value ]
          ]
      ]
        
    let events = 
      let makeEvent kind= 
        Column.column [] [
          yield 
            match kind with 
            | NoEvent ->
              Notification.notification [Notification.Color IsLight] []
            | _ -> 
              let title = string kind 
              Notification.notification [Notification.Color IsWarning] 
                [ 
                  Heading.h3 [] [ str title] 
                ]
        ]
    
      [
        for e in model.Events do 
          yield makeEvent e.Kind
      ]

    Hero.head [ CustomClass "heroPadding"] 
      [
        Columns.columns [] [
          "Food" |> stat model.Stats.Food
          "Hull" |> stat model.Stats.Hull
          "Crew" |> stat model.Stats.Crew
          "Gold" |> stat model.Stats.Gold
        ]
        Columns.columns [] events
      ]

  // TODO handle Midi event to hide notification
  let notification = 
    let active =  model.NotificationMessage.Title.IsSome
    let hide _ = HideNotificationMessage |> dispatch
    let title, message =
      if active then  
        match model.NotificationMessage.Title.Value with 
        | EatCrew msg -> "Ouch!",msg
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
    header
    Hero.body [] knobs
  ]
    
  