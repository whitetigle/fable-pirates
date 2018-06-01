module Types

open Fable.Core

[<Literal>]
let CARDS_COUNT = 24

type Item = 
  | Trap of string
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
  | Solve of Card

type TitleKind = 
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
    Hand:Card list
    ActiveCard:Card option
    Behringer:Behringer.Model 
    NotificationMessage:NotificationMessage
    GameOver:bool
  }
  static member Create (bModel:Behringer.Model) = 
    {
      Hand = []
      ActiveCard=None
      Behringer= bModel
      NotificationMessage=NotificationMessage.Default
      GameOver=false
    }

