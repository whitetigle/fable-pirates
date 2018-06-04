module CardHelper

open Types
open Utils

let fullDeck = [
  Card "Morceau de Beurre";Card "Petite cuillère en argent"
  ;Card "Vieux bout de pain";Card "Vase Ming";Card "Tableau de Maître"
  ;Card "Farine de vers";Card "Stylo Plume en marbre"
  ;Card "Menthe à l'eau";Card "Voiture à friction";Card "Bout de bois flotté"
  ;Card "Chenille orange"
  ;Card "Flacon vert étrange";Card "Poivre de Phu Quoc";Card "Chaussure bleue"
  ;Card "Scarabée en Or";Card "Tomate verte";Card "Feuille d'Acacias"
  ;Card "Confiture d'orange amère";Card "Sucre brun";Card "Serpent d'eau"
  ;Card "Pâtée pour chat"
  ;Card "Fiole de Curry";Card "Une souris verte";Card "Panier en acier"
  ;Card "Médicament dangereux";Card "Rat Plapla";Card "Chien à ressorts"
  ;Card "Poisson Grillé"
  ;Card "Chat Mallow";Card "Carton plein";Card "Herbe Lumineuse"
  ;Card "Grenouille à Pois"
  ;Card "Etoile des neiges";Card "Terre de bruyère";Card "Poupée de cire"
  ;Card "Livre de Magie";Card "Lait Cru";Card "Paille à Son"
  ;Card "Fouet pour jouer"
  ;Card "Oeuf du Bureau";Card "Gateau gourmand";Card "Bol de prière"
  ;Card "Fou-Fourchette";Card "Gant de Boxe";Card "Graine bizarre"
  ;Card "Levure dégonflée"
  ;Card "Montre à rebours";Card "Ver de Mer";Card "Araignée musquée"
  ;Card "Papillon Minute";Card "Courgette jaune";Card "Scorpion à dents"
  ;Card "Guêpe au Miel";Card "Crotte de biquette";Card "Rose des sables"
  ;Card "Fil Indien"
  ;Card "Croquette visqueuse";Card "Verroterie multiple"
  ;Card "Carotte Fessue"
  ;Card "Mulôt têtu";Card "Bougie Rouge";Card "Choux Rave Party"
  ;Card "Petit enfant au coin"
]

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
  let rec getMore (inputDeck:Item list) max (outputDeck:Item list) =
    match outputDeck.Length with 
    | x when x <= max -> 
      let index = (Fable.Import.JS.Math.random() * float inputDeck.Length) |> int 
      let found = inputDeck.[index]
      let ouput = outputDeck @ [found]
      let input = Utils.List.remove index inputDeck
      getMore input max ouput
    | _ -> outputDeck
  
  let items = getMore fullDeck max []

  let cards = 
    items
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

let prepareWishlist max (model:Model) =
  let wanted = 
    let rec select (inputDeck:Card list) max (outputDeck:Id list) =
      match outputDeck.Length with 
      | x when x <= max -> 
        let index = (Fable.Import.JS.Math.random() * float inputDeck.Length) |> int 
        let found = inputDeck.[index]
        let ouput = outputDeck @ [found.Index]
        let input = Utils.List.remove index inputDeck
        select input max ouput
      | _ -> outputDeck
    
    let list = 
      model.Hand
     |> List.filter( fun card ->  
      match card.Item with 
      | Card _ -> true
      | _ -> false  )

    select list max []

  {model with Wanted=wanted;DoneSoFar=wanted}

let reverseHand (model:Model) = 
  let updatedCards = 
    model.Hand 
    |> List.rev
    |> List.map( fun current -> 
      {current with Status=Disabled;Index=model.Rules.CardsCount-current.Index}
    )
  {model with Hand = updatedCards}    

// source: http://www.nutsaboutcoding.com/?p=321
let rotateHand (model:Model) = 
  let spinWheel numTurns (wheel:Card list) =
      wheel 
      |> List.map( fun cur -> cur.Index ) 
      |> List.permute( fun cur -> (cur + numTurns) % wheel.Length )

  let updatedCards = 
    let newOrder = 
      model.Hand 
      |> spinWheel model.Rules.Spin
    
    model.Hand
    |> List.mapi( fun i current -> 
      {current with Status=Disabled;Index=newOrder.[i]}
    )
  {model with Hand = updatedCards}    


let shuffleHand (model:Model) = 
  let updatedCards = 
    model.Hand 
    |> ListShuffle.shuffle
    |> List.map( fun current -> 
      {current with Status=Disabled}
    )
  {model with Hand = updatedCards}    

let swap (model:Model) = 
 
  let updatedCards = 
    let getRandom() = 
      (Fable.Import.JS.Math.random() * float model.Hand.Length) |> int 

    let x,y= getRandom(), getRandom()
    let card1,card2 = model.Hand.[x], model.Hand.[y]
    
    model.Hand 
    |> List.map( fun card -> 
      if card.Index=card1.Index then 
        card2
      else if card.Index = card2.Index then 
        card1
      else 
        card
    )
    |> List.map( fun current -> 
      {current with Status=Disabled}
    )
  {model with Hand = updatedCards}    

let addTraps (model:Model) =
  
  let indexes = 
    [ 0..model.Rules.TrapCount-1]
    |> List.map( fun _ -> 
        (Fable.Import.JS.Math.random() * float model.Hand.Length) |> int 
    )

  let hand = 
    model.Hand
    |> List.map( fun card -> 
      if indexes |> List.exists( fun id -> card.Index = id ) then 
        let rand = Fable.Import.JS.Math.random()
        let trap = 
          match model.Rules.Wanted with 
          | w when w <= 4 -> 
            Knobator "Tourne Boulé !!"
          | w when w <= 8 -> 
            match rand with 
            | x when x >= 0. && x < 0.2 -> Wheel "Roue Infernale !!"
            | _ -> Knobator "Tourne Boulé !!"
          | _ -> 
            match rand with 
//            | x when x >= 0. && x < 0.2 -> Mixator "Thermomixé !!"
            | x when x >= 0.0 && x < 0.5 -> Wheel "Roue Infernale !!"
            | _ -> Knobator "Tourne Boulé !!"
        {card with Item=trap}
      else 
        card
    )

  {model with Hand=hand}  