module CardHelper

open Types
open Utils

let fullDeck = [
  Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre"
  ;Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre"
  ;Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre"
  ;Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre"
  ;Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre"
  ;Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre"
  ;Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre"
  ;Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre"
  ;Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre";Card "Beurre"
]

let flipCard model = 
    let mutable didSomething = false
    model.Hand
    |> List.map( fun card -> 
          let updatedStatus = 
            match card.Status with 
            | Activated -> 
              Disabled
            | Disabled -> 
              didSomething <- true
              Activated
          {card with Status=updatedStatus}
    ),didSomething

let getActiveCard cards= 
  cards 
    |> List.filter( fun card -> 
      card.Status=Activated
    ) 
    |> List.tryHead

let hasCardActive updatedCards = 
  updatedCards 
    |> List.map( fun cards -> 
      cards |> List.filter( fun card -> card.Status=Activated)
    ) 
    |> List.length

let disableAll model=
    let updatedCards = 
      model.Hand
      |> List.map( fun card -> {card with Status=Disabled}
      )
    {model with Hand = updatedCards}

let startingHand max (model:Model) = 
  let rec getMore (inputDeck:Item list) (outputDeck:Item list) =
    match outputDeck.Length with 
    | x when x <= max -> 
      let index = (Fable.Import.JS.Math.random() * float inputDeck.Length) |> int 
      let found = inputDeck.[index]
      let ouput = outputDeck @ [found]
      let input = Utils.List.remove index inputDeck
      getMore input ouput
    | _ -> outputDeck
  
  let cards = 
    getMore fullDeck  []
    |> List.mapi( fun i item -> 
      {
        Index=i
        Status=Disabled
        PreviousValue=0
        TurnLeftCounter=0
        TurnRightCounter= 0
        Item=item
      }
    )
  {model with Hand=cards}

let shuffleHand (model:Model) = 
  let updatedCards = 
    model.Hand 
    |> ListShuffle.shuffle
    |> List.map( fun current -> 
      {current with Status=Disabled}
    )
  {model with Hand = updatedCards}    