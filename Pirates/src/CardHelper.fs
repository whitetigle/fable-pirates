module CardHelper

open Types
open Utils

let private ItemStats : Map<Item,int option> = 
  Map [
    Dinghy, None
    Rope, Some 3
    Harpoon, None
    Monkey, None
    Nothing, None
  ] 

let private Rules : Map<EventKind,Event> =
  Map [
      FloatingCrate,{Kind=FloatingCrate;Required=Some [Harpoon;Rope];WhatsInside=Some [Meat]}
      Island,{Kind=Island;Required=Some[Dinghy];WhatsInside=None}
      Fish,{Kind=Fish;Required=Some[Harpoon;Rope];WhatsInside=Some [Meat]}
      NoEvent,{Kind=NoEvent;Required=None;WhatsInside=None}
      MessageInABottle,{Kind=MessageInABottle;Required=Some[Rope];WhatsInside=None}
      NoEvent,{Kind=NoEvent;Required=None;WhatsInside=None}
      FloatingCrate,{Kind=FloatingCrate;Required=Some [Harpoon;Rope];WhatsInside=None}    
  ]

let startHand = [Dinghy;Rope;Harpoon]

let getCompatibleRules playerItems = 
  let playerSet = playerItems |> set
    
  Rules
  |> Map.filter( fun _ rule -> 
    match rule.Required with 
    | Some required ->
      let requiredSet = required |> set
      requiredSet.IsSubsetOf playerSet
    | None -> false 
  )
  |> Map.toList
  |> List.map( fun (_,rule) -> rule ) 

let flipCard pos model = 
    let mutable didSomething = false
    model.Hand
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
    ),didSomething

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
      model.Hand
      |> List.map( fun cards -> 
        cards
        |> List.map( fun card -> 
            {card with Status=Disabled}
        )
      )
    {model with Hand = updatedCards}

let shuffleHand (cards:Item list) (model:Model) = 
  let realHand = 
    cards
    |> List.map( fun item -> 
      let life = ItemStats.Item item
      match life with 
      | Some life -> 
          [for i in 0..life do yield item]
       | None -> [item]
    )
    |> List.concat

  let newHand = 
    let length = realHand.Length
    let needed = CARDS_COUNT - length
    [for _ in 1..needed do yield Nothing] @ realHand |> ListShuffle.shuffle

  let updatedCards = 
    model.Hand
    |> List.mapi( fun i current -> 
      current
      |> List.mapi( fun j c -> 
        let index = i * 8 + j
        let item = newHand.[index]
        {c with Status=Disabled; Item=Some item}
      )
    )
  {model with Hand = updatedCards}    