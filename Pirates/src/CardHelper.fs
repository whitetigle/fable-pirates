module CardHelper

open Types
open Utils

let getActiveCards updatedCards= 
  let rec getCards output list = 
    match list with 
    | head::tail -> 
      let output = output @ [head]
      getCards output tail  
    | [] -> output
 
  updatedCards 
    |> List.map( fun cards -> 
      cards |> List.filter( fun card -> card.Status=Activated)
    ) 
    |> List.concat
    |> getCards []

let disableAll model=
    let updatedCards = 
      model.Deck
      |> List.map( fun cards -> 
        cards
        |> List.map( fun card -> 
            {card with Status=Disabled}
        )
      )
    {model with Deck = updatedCards}

let shuffleHand (cards:Item list) (model:Model) = 
  let newHand = 
    let length = cards.Length
    let needed = CARDS_COUNT - length
    [for _ in 1..needed do yield Nothing] @ cards |> ListShuffle.shuffle

  let updatedCards = 
    model.Deck
    |> List.mapi( fun i current -> 
      current
      |> List.mapi( fun j c -> 
        let index = i * 8 + j
        {c with Status=Disabled; Item=Some (newHand.[index])}
      )
    )
  {model with Deck = updatedCards}    