module State
open Elmish
open Types
open Fable.Import
open Utils

module StateFlow = 

  // sometimes it's great to debug from the console too
  let printMessage msg model = 
    printfn "%A" msg
    model

  let stopThere model = 
    model, Cmd.none

  let goToNextState next model = 
    model, Cmd.ofMsg next

  let goToNextStates nextStates model = 
    model, Cmd.batch nextStates

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

let checkCardsInHand (model:Model) = 
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

let checkFoodShortage model = 
  if model.Stats.Food <= 0 then 
    if model.Stats.Crew > 1 then 
      let updatedCrew = model.Stats.Crew - 1
      let updatedStats = {model.Stats with Food=EAT_CREW;Crew=updatedCrew}
      let notification  = {model.NotificationMessage with Title=EatCrew "No more food? Let's eat a crew member!" |> Some} 
      { 
        model with 
          Stats = updatedStats
          NotificationMessage=notification
      }    
    else 
      model
  else 
    model    


let displayGameOver (model:Model) = 
  model 
  |> CardHelper.disableAll
  |> StateFlow.goToNextState Msg.GameOver

module Logic = 
  let nextTurn didSomething model =    
    let updatedModel = 
       model
      |> decreaseFood didSomething
      |> checkFoodShortage
    
    if updatedModel.GameOver then
      updatedModel |> displayGameOver
    else
      updatedModel |> checkCardsInHand

let init _ = 
  let bModel,bCmd = Behringer.init()

  Model.Create bModel,
  Cmd.batch [ Cmd.map BehringerMsg bCmd; Cmd.ofMsg StartGame]



let update (msg: Msg) (model: Model) =

  match msg with 
  | HideNotificationMessage -> 
    model
    |> hideMessage
    |> StateFlow.printMessage msg
    |> StateFlow.stopThere

  | Msg.GameOver ->
    model
    |> showMessage (GameOver "Sorry mate! Better luck next time...")
    |> StateFlow.printMessage msg
    |> StateFlow.goToNextState StartGame

  | StartGame -> 
    { model with 
        Stats=Stats.Starting
        Events=EventHelper.NewGame()
    } 
    |> CardHelper.shuffleHand [Dinghy;Rope;Harpoon]
    |> StateFlow.printMessage msg
    |> StateFlow.stopThere
  
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
      model
      |> StateFlow.printMessage msg
      |> StateFlow.stopThere

    else
      model
      |> StateFlow.printMessage msg
      |> StateFlow.stopThere
      
  | FlipCard pos -> 
    
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
    |> StateFlow.printMessage msg
    |> Logic.nextTurn didSomething

  | BehringerMsg bMsg ->
      match bMsg with 
      | Behringer.OnKnob (index, value) -> 
        match model.NotificationMessage.Title with 
        | Some _ -> 
          model 
          |> StateFlow.printMessage msg
          |> StateFlow.goToNextState HideNotificationMessage
        
        | None -> 
          let updatedModel, didSomething = model |> Knob.turn index value
          updatedModel 
          |> StateFlow.printMessage msg
          |> Logic.nextTurn didSomething

      | _ -> 
        let (m, c) = Behringer.update bMsg model.Behringer
        { model with Behringer = m },
        Cmd.map BehringerMsg c
