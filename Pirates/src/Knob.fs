module Knob

open Types 

[<Literal>]
let KNOBSTARTINDEX = 80

[<Literal>]
let KNOB_THRESHOLD = 10

let turn index (value:int option) model = 

  let card = model.Hand.[index]

  // since we can get many model updates we only want to trigger actual test if the 
  // state of the card actually changed
  // Note: hiding a card is free
  let mutable didSomething = false

  let updatedCard = 
    match card.Item with 
    | Nothing -> card
    | _ -> 
      let turnLeft card = 
        let card = {card with TurnRightCounter=0}
        match card.TurnLeftCounter with 
        | times when times > KNOB_THRESHOLD ->
          {card with Status=Disabled}
        | _ -> 
          {card with TurnLeftCounter=card.TurnLeftCounter+1}

      let turnRight card = 
        let card = {card with TurnLeftCounter=0}
        match card.TurnRightCounter with 
        | times when times > KNOB_THRESHOLD ->
          if card.Status = Disabled then 
            didSomething <- true
            {card with Status=Activated}
          else card
        | _ -> 
          {card with TurnRightCounter=card.TurnRightCounter+1}

      let toggle card = 
        let card = {card with TurnLeftCounter=0}
        if card.Status = Disabled then 
          didSomething <- true
          {card with Status=Activated}
        else 
          {card with Status=Disabled}

      let updated = 
        match card.PreviousValue,value with 
        | _, Some v -> 
          match v with 
          | 0 -> turnLeft card
          | 127 -> turnRight card

        | prev, Some newOne when newOne < prev -> turnLeft card                      
        | prev, Some newOne when newOne > prev -> turnRight card
        | _, None -> toggle card
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
          updatedCard
        else
          card
      )


  { model
    with Hand=updatedCards
  }, didSomething
