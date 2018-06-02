module State
open Elmish
open Types
open Fable.Import.Howler
open Fable.Core.JsInterop

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

let hideMessage model = 
  {
    model with 
      NotificationMessage = None 
  }

module Sound =

  module Play = 
    let Trap model = 
      match model.Sounds with 
      | Some sounds ->
        sounds.Trap.play() |> ignore
        model
      | None -> model

    let Good model = 
      match model.Sounds with 
      | Some sounds ->
        sounds.Good.play() |> ignore
        model
      | None -> model

    let Bad model = 
      match model.Sounds with 
      | Some sounds ->
        sounds.Bad.play() |> ignore
        model
      | None -> model

    let Win model = 
      match model.Sounds with 
      | Some sounds ->
        sounds.Win.play() |> ignore
        model
      | None -> model

    let GameTrack model = 
      match model.Sounds with 
      | Some sounds ->
        sounds.GameTrack.play() |> ignore
        model
      | None -> model

  module Stop = 

    let GameTrack model = 
      match model.Sounds with 
      | Some sounds ->
        sounds.GameTrack.stop() |> ignore
        model
      | None -> model
  

module Logic = 
  let nextTurn model =    
    
    match model.Step with 
    | StartGame -> 
      model |> StateFlow.stopThere
    
    | Won -> 
      model |> StateFlow.stopThere

    | GameStarted ->
      model |> checkCardsInHand 

let init sounds _ = 
  let bModel,bCmd = Behringer.init()
  let model = Model.Create bModel
  let model = {model with Sounds=Some sounds}
  model,
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
      |> Sound.Play.GameTrack
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere

  | Msg.StartGame ->    

    { model with Step=GameStarted;ActiveCard=None}
      |> Sound.Stop.GameTrack
      |> CardHelper.startingHand model.Rules.CardsCount
      |> CardHelper.shuffleHand 
      |> CardHelper.prepareWishlist (model.Rules.Wanted-1)
      |> CardHelper.addTraps
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere
  
  | Solve card -> 
    
    match card.Item with 
    | Mixator _ -> 

      { model with GoodMove=Some Trap }
      |> Sound.Play.Trap
      |> CardHelper.reverseHand
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere   

    | Knobator _ -> 

      let updatedRules = 
        let currentRules= model.Rules
        let newThreshold = 
          currentRules.KnobThreshold + currentRules.KnobThresholdIncrease

        {model.Rules with KnobThreshold=newThreshold}

      { model with GoodMove=Some Trap;Rules = updatedRules }
      |> Sound.Play.Trap
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere   

    | Wheel _ -> 
      
      { model with GoodMove=Some Trap }
      |> CardHelper.rotateHand
      |> Sound.Play.Trap
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere   

    | Card _ ->  
      let isGoodCard = 
        model.Wanted 
        |> List.exists ( fun id -> id = card.Index)

      let doneSoFar = 
        if isGoodCard then 
          model.DoneSoFar
          |> List.filter( fun id -> id <> card.Index)
        else 
          model.Wanted
      
      match doneSoFar.IsEmpty with 
      | true -> 
        
        { model with Step=Won }
        |> Sound.Play.Win
        |> StateFlow.logMessage msg
        |> StateFlow.stopThere

      | false -> 
        if isGoodCard then 
          { model with DoneSoFar=doneSoFar;GoodMove=Some Good }
          |> Sound.Play.Good
          |> StateFlow.logMessage msg
          |> StateFlow.stopThere
        
        else 
        
          { model with DoneSoFar=doneSoFar;GoodMove=Some Bad }
          |> Sound.Play.Bad
          |> StateFlow.logMessage msg
          |> StateFlow.stopThere
      
    | _ -> 

      { model with GoodMove=Some Trap }
      |> StateFlow.logMessage msg
      |> StateFlow.stopThere   
      

  | BehringerMsg bMsg ->
      match bMsg with 
      | Behringer.OnKnob (index, value) ->
        
        match model.NotificationMessage with 
        | Some _ -> 
          { model with GoodMove=None } 
          |> StateFlow.logMessage msg
          |> StateFlow.stopThere
          //|> StateFlow.goToNextState HideNotificationMessage
        
        | None -> 
          match model.Step with 
          | StartGame ->

            match index with             
            | x when x >= fst Knob.AMBIENT_DECK && x <= snd Knob.AMBIENT_DECK -> 
              
              let difficulty = index-Knob.AMBIENT_START
              let rules = {model.Rules with Wanted=difficulty}
              { model with Rules = rules }
              |> StateFlow.goToNextState Msg.StartGame
            
            | _ -> 
              model 
              |> StateFlow.stopThere

          | Won ->

            match index with             
            | x when x >= fst Knob.AMBIENT_DECK && x <= snd Knob.AMBIENT_DECK -> 
              
              model
              |> StateFlow.goToNextState Msg.ResetGame
            
            | _ -> 
              model 
              |> StateFlow.stopThere

          | GameStarted ->

            let canCheck = 
              match model.ActiveCard with 
              | Some activeCard -> 
                activeCard.Index <> (index- Knob.KNOBSTARTINDEX)
              | None -> true

            if not canCheck then 
              model 
              |> StateFlow.stopThere
            else              
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
                let updatedModel= model |> Knob.turn index value

                { updatedModel with GoodMove=None}
                |> StateFlow.logMessage msg
                |> Logic.nextTurn 
            
              | _ -> 
                { model with GoodMove=None }  
                |> StateFlow.stopThere
          | _ -> 
            { model with GoodMove=None }  
            |> StateFlow.stopThere

      | _ -> 
        let (m, c) = Behringer.update bMsg model.Behringer
        { model with Behringer = m },
        Cmd.map BehringerMsg c
