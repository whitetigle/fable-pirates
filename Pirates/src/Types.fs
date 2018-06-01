module Types

open Fable.Core

[<Literal>]
let CARDS_COUNT = 24

type Id = int

type Item = 
  | Repetor of string // Repeats 5 times the same object
  | Mixator of string // shuffles everything
  | Colorator of string // Colorator : colors texts
  | Backgroundator of string // Colorator : colors background
  | Knobator of string // changes knob response time
  | Card of string
  | Nothing

[<StringEnum>]
type CardStatus = Activated | Disabled

type Card = 
  {
    Index:int
    Status:CardStatus
    PreviousValue:int
    TurnLeftCounter:int
    TurnRightCounter:int
    Item:Item
  }

type RenderMsg = Render
type Msg= 
  | BehringerMsg of Behringer.Msg
  | FlipCard 
  | HideNotificationMessage
  | GameOver
  | StartGame
  | ResetGame
  | Solve of Card

type TitleKind = 
  | GameOver of string

type NotificationMessage =
  {
    Title:TitleKind
  }
  static member Default = 
    {
      Title=GameOver "OOPS"
    }

type EndOfGame = 
  | Won 
  | Lost 

type Rules = 
  {
    KnobThreshold : int
  }
  static member Prepare = {
    KnobThreshold=30
  }

type Model = 
  { 
    Hand:Card list
    Wanted:Id list
    ActiveCard:Card option
    Behringer:Behringer.Model 
    NotificationMessage:NotificationMessage option
    EndOfGame:EndOfGame option
    Rules:Rules
  }
  static member Create (bModel:Behringer.Model) = 
    {
      Hand = []
      Wanted=[]
      ActiveCard=None
      Behringer= bModel
      NotificationMessage=None
      EndOfGame=None
      Rules=Rules.Prepare
    }

