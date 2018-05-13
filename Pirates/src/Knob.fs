module Knob

open Types 

[<Literal>]
let KNOBSTARTINDEX = 80

[<Literal>]
let KNOB_THRESHOLD = 10

let turn index value model = 
  let realIndex = index - KNOBSTARTINDEX
  let width = MAXX + 1

  let x,y = 
    let y = realIndex % width
    let y = if y = 0 then MAXX else y - 1          
    let x = realIndex / width
    let x = if y = MAXX then x - 1 else x
    x,y

  let card = model.Hand.[x].[y]

  // since we can get many model updates we only want to trigger actual test if the 
  // state of the card actually changed
  // Note: hiding a card is free
  let mutable didSomething = false

  let updatedCard = 
    match card.Item with 
    | Some item -> 
      match item with 
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

        let updated = 
          match card.PreviousValue,value with 
          | _, 0 -> turnLeft card

          | _, 127 -> turnRight card

          |  prev, newOne when newOne < prev -> turnLeft card            
          
          | prev, newOne when newOne > prev -> turnRight card

          | _ -> card
        
        { updated with PreviousValue=value}
    | None -> card

  let updatedCards = 
    model.Hand
    |> List.mapi( fun i cards -> 
          cards
          |> List.mapi( fun j card -> 
            if i=x && j=y then 
              updatedCard
            else
              card
          )
    )

  { model
    with Hand=updatedCards
  }, didSomething
