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
  let notification = Some {Title=title} 
  { model with NotificationMessage=notification}

let hideMessage model = 
  {
    model with 
      NotificationMessage = None 
  }


module Logic = 
  let nextTurn model =    
    
    match model.Step with 
    | StartGame -> 
      model |> StateFlow.stopThere
    
    | Won -> 
      model |> StateFlow.stopThere

    | GameStarted ->
      model |> checkCardsInHand 

let init _ = 
  let bModel,bCmd = Behringer.init()
  Model.Create bModel,
  Cmd.batch [ Cmd.map BehringerMsg bCmd; Cmd.ofMsg Msg.ResetGame]


let update (msg: Msg) (model: Model) =

  match msg with 
  | HideNotificationMessage -> 
    model
      |> hideMessage
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere

  | ResetGame ->    
    { model with Step=StartGame;ActiveCard=None}
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere
      //|> StateFlow.goToNextState StartGame

  | Msg.StartGame ->    

    { model with Step=GameStarted;ActiveCard=None}
      |> CardHelper.startingHand model.Rules.CardsCount
      |> CardHelper.shuffleHand 
      |> CardHelper.prepareWishlist (model.Rules.Wanted-1)
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere
  
  | Solve card -> 
    
    let isGoodCard = 
      model.Wanted 
      |> List.exists ( fun id -> id = card.Index)

    let doneSoFar = 
      if isGoodCard then 
        printfn "Found one ! %i" card.Index
        model.DoneSoFar
        |> List.filter( fun id -> id <> card.Index)
      else 
        printfn "Error !"
        model.Wanted
    
    printfn "Done so far %A" doneSoFar
    match doneSoFar.IsEmpty with 
    | true -> 
      
      { model with Step=Won }
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere

    | false -> 
      { model with DoneSoFar=doneSoFar }
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere
      
  | BehringerMsg bMsg ->
      match bMsg with 
      | Behringer.OnKnob (index, value) -> 
        match model.NotificationMessage with 
        | Some _ -> 
          model 
          |> StateFlow.logMessage msg
          |> StateFlow.stopThere
          //|> StateFlow.goToNextState HideNotificationMessage
        
        | None -> 
          match model.Step with 
          | GameStarted ->
            match index with 
            | x when x >= fst Knob.CONTROL_DECK && x <= snd Knob.CONTROL_DECK -> 
              printfn "Control deck %i" index
              model 
              |> StateFlow.logMessage msg
              |> Logic.nextTurn 

            | x when x >= fst Knob.AMBIENT_DECK && x <= snd Knob.AMBIENT_DECK -> 
              printfn "Ambient deck %i" index
              model 
              |> StateFlow.logMessage msg
              |> Logic.nextTurn 

            | x when x >= fst Knob.CARD_DECK && x <= snd Knob.CARD_DECK -> 
              let updatedModel, didSomething = model |> Knob.turn index value model.Rules.KnobThreshold

              updatedModel 
              |> StateFlow.logMessage msg
              |> Logic.nextTurn 
          
            | _ -> 
              model 
              |> StateFlow.stopThere
          | _ -> 
            model 
            |> StateFlow.stopThere

      | _ -> 
        let (m, c) = Behringer.update bMsg model.Behringer
        { model with Behringer = m },
        Cmd.map BehringerMsg c
