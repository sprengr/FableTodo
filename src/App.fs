module App

open Elmish
open Elmish.React
open Feliz


type State = {
    Text: string
}

let init () =
    { Text = "hello from fable"}

type Msg = 
    | Dummy

let update (msg: Msg) (state: State): State =
    match msg with
    | Dummy -> state

let render (state: State) (dispatch: Msg -> unit) =
    Html.div
        [ prop.text state.Text ]

Stylesheet.apply "./styles/main.scss"

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
