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
    let updatedModel = 
       model
    
    match updatedModel.EndOfGame with 
    | Some eof -> 
      { updatedModel with EndOfGame=Some eof}
      |> StateFlow.stopThere

    | None -> 
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

  | ResetGame ->    
    { model with EndOfGame=None;ActiveCard=None}
      |> StateFlow.logMessage msg
      |> StateFlow.goToNextState StartGame

  | StartGame ->    

    model
      |> CardHelper.startingHand CARDS_COUNT
      |> CardHelper.shuffleHand 
      |> CardHelper.prepareWishlist 1
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere
  
  | Solve card -> 
    
    let wanted = 
      model.Wanted
      |> List.filter( fun id -> id <> card.Index)
    
    match wanted.IsEmpty with 
    | true -> 
      
      { model with EndOfGame=Some Won }
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere

    | false -> 
      { model with Wanted=wanted }
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere
      
  | FlipCard -> 
    
    let updatedCards, didSomething = 
      model |> CardHelper.flipCard
    
    { model with Hand=updatedCards} 
    |> StateFlow.logMessage msg
    |> Logic.nextTurn 

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

            printfn "%A%A" model.EndOfGame didSomething
            match model.EndOfGame, didSomething with 
            | Some _, true -> 

              model
              |> StateFlow.logMessage msg
              |> StateFlow.goToNextState ResetGame
           
            | Some _, false -> 
              model 
              |> StateFlow.logMessage msg
              |> StateFlow.stopThere
            
            | _ -> 
              updatedModel 
              |> StateFlow.logMessage msg
              |> Logic.nextTurn 
          
          | _ -> 
            printfn "index %i" index
            model 
            |> StateFlow.stopThere

      | _ -> 
        let (m, c) = Behringer.update bMsg model.Behringer
        { model with Behringer = m },
        Cmd.map BehringerMsg c
