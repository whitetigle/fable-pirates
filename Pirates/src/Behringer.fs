module Behringer

open Fable.Import.WebMIDI

 [<Literal>]
let INPUT = "input-0"

type Alert =
  | Info of string
  | Success of string
  | Warning of string
  | Error of string

let DoNothing _ = ()

type Model =  
  { 
      MIDIInputs: (string*IMIDIInput) list
      SelectedMIDIInput: string option
      MIDIAccess: IMIDIAccess option
      IsMIDIEnabled: bool
      Messages: Alert list 
      Counter:int
}

type Index = int
type KnobValue = int

type Msg = 
  | MIDIConnected of IMIDIAccess     // MIDI successfully connected
  | MIDIStateChange of IMIDIAccess                  // MIDI successfully connected
  | MIDIError of exn                 // Error connecting MIDI
  | Message of Alert                 // A message
  | InputSelected of string
  | SendNote                        // Send a MIDI note
  | OnMessage of byte array
  | OnKnob of Index * KnobValue option

open Elmish
open Fable.Import

let init () : Model*Cmd<Msg> =
    { 
      MIDIInputs = []
      SelectedMIDIInput = None
      MIDIAccess = None
      IsMIDIEnabled = false
      Messages = [] 
      Counter=0}, Cmd.ofPromise MIDI.requestAccess [ Sysex true ] MIDIConnected MIDIError

[<RequireQualifiedAccess>]
module JSMap =
    let toList (m: JS.Map<'key, 'value>): ('key * 'value) list =
        let mutable result = []
        m.forEach (fun value key _ -> result <- (key,value)::result) 
        result


let update (msg:Msg) (model:Model) : Model*Cmd<Msg> =    
    let success = Success >> Message >> Cmd.ofMsg
    let info = Info >> Message >> Cmd.ofMsg
    let error = Error >> Message >> Cmd.ofMsg
    
    match msg with
    | MIDIConnected midiAccess -> 

        let stateChangeSub dispatch =
          let onStateChange _ =
            dispatch (MIDIStateChange midiAccess)
          midiAccess.OnStateChange <- onStateChange

        { model with MIDIAccess = Some midiAccess
                     IsMIDIEnabled = true }, Cmd.batch [ success "MIDI connected"
                                                         Cmd.ofSub stateChangeSub
                                                         Cmd.ofMsg (MIDIStateChange midiAccess)
                                                         Cmd.ofMsg (InputSelected INPUT)]
    | MIDIStateChange midiAccess->

        let getSelectedId map selected =
          match map |> Map.toList with
          | [] -> None
          | (id, _)::rest -> 
            match selected with
            | (Some oId) when (oId = id) || (rest |> List.exists (fun (key, _) -> oId = key)) -> Some oId
            | _ -> Some id
        
        let inputs = 
            midiAccess.Inputs 
            |> Map.toList 
            |> List.filter (fun (_, i) -> i.Name |> Option.filter (fun v -> v <> "") |> Option.isSome)
        
        let selectedInput = getSelectedId midiAccess.Inputs None
        
        { model with 
             MIDIInputs = inputs
             SelectedMIDIInput = selectedInput 
                     }, info "State changed"

    | MIDIError ex ->
        { model with MIDIAccess = None                     
                     MIDIInputs = []
                     IsMIDIEnabled = false
                     SelectedMIDIInput = None }, error ex.Message

    | Message alert -> 
      printfn "%A" alert 
      model, Cmd.none

    | OnMessage data -> 
        printfn "MIDI message received at timestamp [ %i bytes]" data.Length
        let status = int data.[0]
        let command = (int data.[0]) >>> 4
        let channel = (int data.[1]) &&& 0x7F
        let velocity = (int data.[2]) &&& 0x7F


        (*
        #if DEBUG
        printfn "%08X %08X %08X %08X" status command channel velocity
        printfn "%i %i %i %i" status command channel velocity
        #endif
        
        printfn "counter %i" model.Counter
        *)
        
        { model with Counter=model.Counter+1}, Cmd.ofMsg <| OnKnob (channel, Some velocity)

    | InputSelected id ->

        printfn "%s" id

        let onMidiMessageSub (dispatch: Dispatch<Msg>) = 
            let onMidiMessage (ev: WebMIDI.IMIDIMessageEvent) =
                dispatch (OnMessage ev.Data)
            
            match id, model.SelectedMIDIInput with
                  | ("", Some oldId) ->
                    match model.MIDIInputs |> List.tryFind (fun (i, _)  -> i = oldId) |> Option.map snd with
                    | Some oldInput -> oldInput.OnMidiMessage <- DoNothing
                    | None -> ()
                  | (id, None) when id <> "" ->
                    match model.MIDIInputs |> List.tryFind (fun (i, _)  -> i = id) |> Option.map snd with
                    | Some i -> 
                      i.OnMidiMessage <- onMidiMessage
                    | None -> ()
                  | (id, Some oldId) when oldId <> id ->
                    match model.MIDIInputs |> List.tryFind (fun (i, _)  -> i = oldId) |> Option.map snd with
                    | Some oldInput -> oldInput.OnMidiMessage <- DoNothing
                    | None -> ()

                    match model.MIDIInputs |> List.tryFind (fun (i, _)  -> i = id) |> Option.map snd with
                    | Some i -> 
                      Browser.console.log i
                      i.OnMidiMessage <- onMidiMessage
                    | None -> ()
                  | _ -> ()        

        let model = 
            { model with 
                SelectedMIDIInput = 
                       match id with 
                       | "" -> None 
                       | id -> Some id }

        model, Cmd.ofSub onMidiMessageSub

     (*
    | SendNote -> 
        match model.MIDIAccess, model.SelectedMIDIOutput with
        | Some midi, Some out -> model, Cmd.ofFunc (sendNote midi) out (fun _ -> Message (Success "sent")) (fun ex -> Message (Error ex.Message))
        | Some _, None -> model, error "No Output"
        | _, _ -> model, error "No MIDI connection"
        *)

