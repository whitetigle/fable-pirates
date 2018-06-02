module View 

open Types
open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome
open Fulma.Extensions

module Inside = GoGoFulma.Inside
open GoGoFulma.ReactHelpers

let root (model:Model) dispatch= 

  match model.Step with 

  | StartGame -> 

    Hero.hero [Hero.Color IsLight; Hero.IsFullHeight] [
      Container.container 
        [
          Container.IsFluid
          Container.Modifiers [Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]
        ] 
        [ 
           "Le Professeur Esclottes a perdu la mémoire !" |> Inside.Str => Inside.Heading.h1
           "Aidez-le à retrouver les objets perdus dans son armoire !" |> Inside.Str => Inside.Heading.h2
           "Tournez les manivelles pour ouvrir les tiroirs !" |> Inside.Str => Inside.Heading.h3
           "Attention... Des pièges se cachent dans le noir.." |> Inside.Str => Inside.Heading.h4
        ]
        => Inside.Hero.Body
    ]    

  | Won -> 

    let title, textColor, backColor = 
      "Gagné !", "has-text-success", IsLight

    let card = 
      div[ ClassName (sprintf "label %s" textColor)] [         
        title |> Inside.Str
      ]

    Hero.hero [Hero.Color backColor; Hero.IsFullHeight] [
      Container.container 
        [
          Container.IsFluid
          Container.Modifiers [Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]          
        ] 
        [ 
          card 
          "Vous avez trouvé tous les objets dans l'armoire !" |> Inside.Str => Inside.Heading.h1
          "Le Professeur vous remercie !" |> Inside.Str => Inside.Heading.h2
          "Il vous prie de ne pas lui en vouloir !" |> Inside.Str => Inside.Heading.h3
          "La prochaine fois il rangera mieux ses tiroirs !" |> Inside.Str => Inside.Heading.h3
          "À bientôt et Au revoir ! " |> Inside.Str => Inside.Heading.h4
        ]
        => Inside.Hero.Body
    ]
  
  | GameStarted -> 

    let head = 
      let l = model.Rules.Wanted - 1
      let container = 
        match l with
        | x when x < 5 -> 
          Inside.ColumnWithSize.Is9
        | x when x < 9 -> 
          Inside.ColumnWithSize.Is9
        |_ -> 
          Inside.ColumnWithSize.Is7
        
      let stars = 
        let foundStars = model.DoneSoFar.Length
        [0..l] 
        |> List.mapi( fun i _ ->
          if i < foundStars then  
            Icon.faIcon [ Icon.CustomClass "BigStar" ] [ Fa.icon Fa.I.StarO ]
            => Inside.ColumnWithSize.Is1
          else 
            Icon.faIcon [ Icon.CustomClass "BigStar"  ] [ Fa.icon Fa.I.Star ]
            => Inside.ColumnWithSize.Is1
        ) |> Inside.Columns


      Hero.head [] [
         [ 
           [] |> container
           stars => Inside.ColumnWithSize.Is3
         ]          
         |> Inside.Columns
      ]

    let card = 
      match model.ActiveCard with 
      | Some card ->
        let label, color = 
          match card.Item with 
          | Card label -> label, "has-text-dark"
          | Mixator label -> label, "has-text-danger" 
          | Knobator label -> label, "has-text-danger" 
          | Wheel label -> label, "has-text-danger" 
          | Nothing -> "", "has-text-dark"

        div[ ClassName (sprintf "label %s" color)] [         
          label |> Inside.Str
        ]

      | None -> str "" 

    let color = 
      match model.GoodMove with 
      | None -> IsDark
      | Some Bad -> IsDanger 
      | Some Good -> IsSuccess
      | Some Trap -> IsDark

    Hero.hero [Hero.Color color; Hero.IsFullHeight] [
      head
      Container.container 
        [Container.IsFluid; Container.Modifiers [Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]] 
        [ card ]
        => Inside.Hero.Body
    ]
      
    