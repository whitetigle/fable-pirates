module State
open Elmish
open Types

module StateFlow = 

  // sometimes it's great to debug from the console too
  let logMessage msg model = 
    printfn "%A" msg
    model

  let stopThere model = 
    model, Cmd.none

  let goToNextState next model = 
    model, Cmd.ofMsg next

  let goToNextStates nextStates model = 
    model, Cmd.batch nextStates

let checkCardsInHand (model:Model) = 
  let found = model.Hand |> CardHelper.getActiveCard

  let command = 
    if found.IsSome then Cmd.ofMsg (Solve found.Value) else Cmd.none
  
  { model with ActiveCard=found}, command

let showMessage title model =
  let notification  = {model.NotificationMessage with Title=title |> Some } 
  { model with NotificationMessage=notification}

let hideMessage model = 
  {
    model with 
      NotificationMessage = {model.NotificationMessage with Title=None}
  }

let displayGameOver (model:Model) = 
  model 
  |> CardHelper.disableAll
  |> StateFlow.goToNextState Msg.GameOver

module Logic = 
  let nextTurn didSomething model =    
    let updatedModel = 
       model
    
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
    |> StateFlow.logMessage msg
    |> StateFlow.stopThere

  | Msg.GameOver ->
    model
    |> showMessage (GameOver "Sorry mate! Better luck next time...")
    |> StateFlow.logMessage msg
    |> StateFlow.goToNextState StartGame

  | StartGame ->    
    model
    |> CardHelper.startingHand CARDS_COUNT
    |> CardHelper.shuffleHand 
    |> StateFlow.logMessage msg
    |> StateFlow.stopThere
  
  | Solve hand -> 
    
    (*
    let requested = 
      hand 
      |> List.map( fun card -> string card.Item )
      |> List.sort
      |> set

    printfn "requested %A" requested

    let outcomes = 
      model.Events
      |> List.filter( fun event -> 
        match event.Required with 
        | Some list -> 
          let current = 
            list
            |> List.map( fun item -> string item)
            |> List.sort
            |> set
          
          current.IsSubsetOf requested
        | None -> 
          false
      )
    printfn "outcomes %A" outcomes

    if outcomes.Length > 0 then
      //let 
      printfn "we got some cards %A" outcomes
      model
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere

    else*)
      model
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere
      
  | FlipCard -> 
    
    let updatedCards, didSomething = 
      model |> CardHelper.flipCard
    
    { model with Hand=updatedCards} 
    |> StateFlow.logMessage msg
    |> Logic.nextTurn didSomething

  | BehringerMsg bMsg ->
      match bMsg with 
      | Behringer.OnKnob (index, value) -> 
        match model.NotificationMessage.Title with 
        | Some _ -> 
          model 
          |> StateFlow.logMessage msg
          |> StateFlow.goToNextState HideNotificationMessage
        
        | None -> 
          match index with 
          | x when x >= fst Knob.CONTROL_DECK && x <= snd Knob.CONTROL_DECK -> 
            printfn "Control deck %i" index
            model 
            |> StateFlow.logMessage msg
            |> Logic.nextTurn true

          | x when x >= fst Knob.AMBIENT_DECK && x <= snd Knob.AMBIENT_DECK -> 
            printfn "Ambient deck %i" index
            model 
            |> StateFlow.logMessage msg
            |> Logic.nextTurn true

          | x when x >= fst Knob.CARD_DECK && x <= snd Knob.CARD_DECK -> 
            printfn "Card deck %i" index
            let updatedModel, didSomething = model |> Knob.turn index value
            updatedModel 
            |> StateFlow.logMessage msg
            |> Logic.nextTurn didSomething
          
          | _ -> 
            printfn "index %i" index
            model 
            |> StateFlow.stopThere

      | _ -> 
        let (m, c) = Behringer.update bMsg model.Behringer
        { model with Behringer = m },
        Cmd.map BehringerMsg c
