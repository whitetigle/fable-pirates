module Types

open Fable.Core
open Fable.Import.Howler

type Id = int

type Item = 
  | Mixator of string // shuffles everything
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
  | HideNotificationMessage
  | StartGame
  | ResetGame
  | Solve of Card

type NotificationMessage = 
  | GoodMove of string
  | BadMove of string

type MoveKind = 
  | Good
  | Bad 
  | Trap

type Step = 
  | Won 
  | StartGame
  | GameStarted

type Sounds = {
  Knob:Howl
}

type Rules = 
  {
    KnobThreshold : int
    CardsCount:int
    Wanted:int
    TrapCount:int
    KnobThresholdIncrease:int
  }
  static member Prepare = {
    KnobThreshold=30
    CardsCount=24
    Wanted=9
    TrapCount=2
    KnobThresholdIncrease=40
  }

type Model = 
  { 
    Hand:Card list
    Wanted:Id list
    DoneSoFar:Id list
    ActiveCard:Card option
    Behringer:Behringer.Model 
    NotificationMessage:NotificationMessage option
    Step:Step
    Rules:Rules
    GoodMove:MoveKind option
    Sounds:Sounds option
  }
  static member Create (bModel:Behringer.Model) = 
    {
      Hand = []
      Wanted=[]
      DoneSoFar=[]
      ActiveCard=None
      Behringer= bModel
      NotificationMessage=None
      Step=StartGame
      Rules=Rules.Prepare
      GoodMove=None
      Sounds=None
    }

