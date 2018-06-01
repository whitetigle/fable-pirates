module Types

open Fable.Core

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

type Step = 
  | Won 
  | StartGame
  | GameStarted

type Rules = 
  {
    KnobThreshold : int
    CardsCount:int
    Wanted:int
  }
  static member Prepare = {
    KnobThreshold=30
    CardsCount=24
    Wanted=4
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
    }

