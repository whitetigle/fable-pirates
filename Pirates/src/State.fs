module State
open Elmish
open Types
open Fable.Import
open Utils


module EventHelper = 

  let NewGame() = 
    [
      {Kind=FloatingCrate;Required=Some [Harpoon;Rope];WhatsInside=Some [Meat]}
      {Kind=Island;Required=Some[Dinghy];WhatsInside=None}
      {Kind=Fish;Required=Some[Harpoon;Rope];WhatsInside=Some [Meat]}
      {Kind=NoEvent;Required=None;WhatsInside=None}
      {Kind=MessageInABottle;Required=Some[Rope];WhatsInside=None}
      {Kind=NoEvent;Required=None;WhatsInside=None}
      {Kind=FloatingCrate;Required=Some [Harpoon;Rope];WhatsInside=None}
    ]

  let AddEvent list = 
    let newEvent = {Kind=FloatingCrate;Required=Some[Harpoon;Harpoon;Harpoon];WhatsInside=None}
    let list = list |> List.tail
    list @ [newEvent]

let checkCardsInHand model = 
  let found = model.Deck |> CardHelper.getActiveCards
  let countFound = found.Length
  if countFound >= 1 then 
    model, Cmd.ofMsg (Solve found)
  else
    model, Cmd.none

let showMessage title model =
  let notification  = {model.NotificationMessage with Title=title |> Some } 
  { model with NotificationMessage=notification}

let hideMessage model = 
  {
    model with 
      NotificationMessage = {model.NotificationMessage with Title=None}
  }

let decreaseFood canDoIt model =
  if canDoIt then 
    let updatedFood = 
      let newState = model.Stats.Food - 1
      if newState <= 0 then 0 else newState
    {model with Stats = { model.Stats with Food = updatedFood}}
  else model 

let stopThere model = 
  model, Cmd.none

let goToNextState next model = 
  model, Cmd.ofMsg next

let init _ = 
  let bModel,bCmd = Behringer.init()

  Model.Create bModel,
  Cmd.batch [ Cmd.map BehringerMsg bCmd; Cmd.ofMsg StartGame]



let update (msg: Msg) (model: Model) =

  match msg with 
  | HideNotificationMessage -> 
    model
    |> hideMessage
    |> stopThere

  | Msg.GameOver ->
    model
    |> showMessage (GameOver "Sorry mate! Better luck next time...")
    |> goToNextState StartGame

  | StartGame -> 
    { model with 
        Stats=Stats.Starting
        Events=EventHelper.NewGame()
    } 
    |> CardHelper.shuffleHand [Dinghy;Rope;Harpoon]
    |> stopThere
  
  | Solve hand -> 
    
    let requested = 
      hand 
      |> List.filter( fun card ->  card.Item.IsSome )
      |> List.map( fun card -> string card.Item.Value )
      |> List.sortDescending
      |> String.concat ""

    printfn "requested %A" requested

    let outcomes = 
      model.Events
      |> List.filter( fun event -> 
        match event.Required with 
        | Some list -> 
          let current = 
            list
            |> List.map( fun item -> string item)
            |> List.sortDescending
            |> String.concat ""
          
          current = requested
        | None -> 
          false
      )
    printfn "outcomes %A" outcomes

    if outcomes.Length > 0 then
      //let 
      printfn "we got some cards %A" outcomes
      model, Cmd.none
    else
      model
      |> stopThere
      
  | ToggleCard pos -> 
    
    let mutable didSomething = false
    let updatedCards = 
      model.Deck
      |> List.mapi( fun i cards -> 
        cards
        |> List.mapi( fun j card -> 
          if i=pos.X && j = pos.Y then 
            match card.Item with 
            | Some item ->
              match item with 
              | Nothing -> card
              | _ ->  
                let updatedStatus = 
                  match card.Status with 
                  | Activated -> 
                    Disabled
                  | Disabled -> 
                    didSomething <- true
                    Activated
                {card with Status=updatedStatus}
            | None -> card
          else
            card
        )
      ) 
    
    { model with Deck=updatedCards} 
    |> decreaseFood didSomething
    |> checkCardsInHand
      (*
      let found = updatedCards |> CardHelper.getActiveCards
      let countFound = found.Length
      if countFound >= 1 then 
        { model with Deck=updatedCards}, Cmd.ofMsg (Solve found)
      else
        let remainingFood = model.Stats.Food - 1
        if remainingFood <= 0 then 
          if model.Stats.Crew > 1 then 
            let updatedCrew = model.Stats.Crew - 1
            let updatedStats = {model.Stats with Food=EAT_CREW;Crew=updatedCrew}
            let notification  = {model.NotificationMessage with Title=EatCrew "No more food? Let's eat a crew member!" |> Some} 
            { 
              model with 
                Stats = updatedStats
                NotificationMessage=notification
                Deck=updatedCards
                //Events=EventHelper.AddEvent model.Events
            },Cmd.none
          else 
            { model with Deck=updatedCards} |> hideCards, Cmd.ofMsg Msg.GameOver
        else 
          let updatedStats = {model.Stats with Food=remainingFood}
          { 
            model with 
              Stats = updatedStats
              Deck=updatedCards
              //Events=EventHelper.AddEvent model.Events
          }, Cmd.none*)

  | BehringerMsg bMsg ->
      match bMsg with 
      | Behringer.OnKnob (index, value) -> 
        let updatedModel, didSomething = model |> Knob.turn index value
        updatedModel 
        |> decreaseFood didSomething
        |> checkCardsInHand

      | _ -> 
        let (m, c) = Behringer.update bMsg model.Behringer
        { model with Behringer = m },
        Cmd.map BehringerMsg c
