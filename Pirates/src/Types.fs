module Types

open Fable.Core

[<Literal>]
let MAXY = 2

[<Literal>]
let MAXX = 7

type Position = 
  {
    X:int
    Y:int
  }

[<Literal>]
let crew = 1

[<Literal>]
let EAT_CREW = 3

[<Literal>]
let CARDS_COUNT = 24

type Stats = 
  {
    Food:int
    Hull:int
    Crew:int
    Gold:int
  }
  static member Starting =     
    {
      Food=crew*EAT_CREW
      Hull=5
      Crew=crew
      Gold=0
    }

[<StringEnum>]
type Item = 
  | Harpoon
  | Nothing
  | Rope
  | Dinghy
  | Monkey

[<StringEnum>]
type Gift =
  | Meat
  | Equipment

[<StringEnum>]
type EventKind =
  | FloatingCrate
  | MessageInABottle
  | Island
  | Fish
  | NoEvent

type Event = 
  {
    Kind:EventKind
    Required: Item list option
    WhatsInside: Gift list option
  }

[<StringEnum>]
type CardStatus = Activated | Disabled

type Card = 
  {
    Status:CardStatus
    PreviousValue:int
    TurnLeftCounter:int
    TurnRightCounter:int
    Pos:Position
    Item:Item option
  }
  static member Start pos = 
    {
      Status = Disabled
      PreviousValue=0
      TurnLeftCounter=0
      TurnRightCounter=0
      Pos=pos
      Item=None
    }

type RenderMsg = Render
type Msg= 
  | BehringerMsg of Behringer.Msg
  | FlipCard of Position
  | HideNotificationMessage
  | GameOver
  | StartGame
  | Solve of Card list

type TitleKind = 
  | EatCrew of string 
  | GameOver of string

type NotificationMessage =
  {
    Title:TitleKind option
  }
  static member Default = 
    {
      Title=None
    }

type Model = 
  { 
    Deck:Card list list
    Behringer:Behringer.Model 
    Stats:Stats
    NotificationMessage:NotificationMessage
    Events:Event list
    GameOver:bool
  }
  static member Create (bModel:Behringer.Model) = 
    let knobs = 
        [0..MAXY] 
        |> List.mapi( fun i _ -> 
            [0..MAXX] 
            |> List.mapi( fun j _ -> Card.Start {X=i;Y=j}
          )
        )
    {
      Deck = knobs
      Behringer= bModel
      Stats=Stats.Starting
      NotificationMessage=NotificationMessage.Default
      Events=[]
      GameOver=false
    }