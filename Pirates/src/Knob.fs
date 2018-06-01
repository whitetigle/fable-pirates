module Knob

open Types 

[<Literal>]
let KNOBSTARTINDEX = 80

let CONTROL_DECK = 1,8
let AMBIENT_DECK = 65,80
let CARD_DECK = 81,104

let turn index (value:int option) knb model = 

  let index = index - KNOBSTARTINDEX

  // since we can get many model updates we only want to trigger actual test if the 
  // state of the card actually changed
  // Note: hiding a card is free
  let mutable didSomething = false

  let updatedCard card= 
    match card.Item with 
    | Nothing -> 
      card

    | _ -> 
      let turnLeft card = 
        let card = {card with TurnRightCounter=0}
        match card.TurnLeftCounter with 
        | times when times > knb ->
          if card.Status <> Disabled then 
            didSomething <- true
            {card with Status=Disabled}
          else card
        | _ -> 
          {card with TurnLeftCounter=card.TurnLeftCounter+1}

      let turnRight card = 
        let card = {card with TurnLeftCounter=0}
        match card.TurnRightCounter with 
        | times when times > knb ->
          if card.Status = Disabled then 
            didSomething <- true
            {card with Status=Activated}
          else card
        | _ -> 
          {card with TurnRightCounter=card.TurnRightCounter+1}

      let updated = 

        match card.PreviousValue,value with 
        | _, Some v when v = 0  ->
           turnLeft card
        | _, Some v when v = 127  -> 
           turnRight card
        | prev, Some newOne when newOne < prev -> 
          turnLeft card                      
        | prev, Some newOne when newOne > prev -> 
          turnRight card
        | _ -> card
      
      match value with 
      | Some v -> 
        { updated with PreviousValue=v} 
      | None -> 
        updated

  let updatedCards = 
    model.Hand
    |> List.map( fun card -> 
        if card.Index = index then 
          updatedCard card
        else
          { card with Status=Disabled; PreviousValue=0;TurnRightCounter=0;TurnLeftCounter=0 }
      )

  { model
    with Hand=updatedCards
  }, didSomething
