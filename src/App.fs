module App

open Elmish
open Elmish.React
open Feliz

type Todo =
    { Id: int
      Description: string
      Completed: bool }

type TodoBeingEdited = { Id: int; Description: string }

type State =
    { TodoList: Todo list
      NewTodo: string
      TodoBeingEdited: TodoBeingEdited option }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of int
    | DeleteTodo of int
    | CancelEdit
    | ApplyEdit
    | StartEditingTodo of int
    | SetEditedDescription of string

let init () =
    { TodoBeingEdited = None
      TodoList =
          [ { Id = 1
              Description = "Learn F#"
              Completed = false }
            { Id = 2
              Description = "Learn Elmish"
              Completed = false } ]
      NewTodo = "" }

let update (msg: Msg) (state: State): State =
    match msg with
    | SetNewTodo newTodo -> { state with NewTodo = newTodo }

    | AddNewTodo when state.NewTodo = "" -> state

    | AddNewTodo ->
        let nextId =
            match state.TodoList with
            | [] -> 1
            | elems ->
                elems
                |> List.maxBy (fun todo -> todo.Id)
                |> fun todo -> todo.Id + 1

        let nextTodo =
            { Id = nextId
              Description = state.NewTodo
              Completed = false }

        { state with
              TodoList = List.append state.TodoList [ nextTodo ] }

    | ToggleCompleted todoId ->
        let nextTodoList =
            state.TodoList
            |> List.map (fun todo ->
                if todo.Id = todoId then
                    { todo with
                          Completed = not todo.Completed }
                else
                    todo)

        { state with TodoList = nextTodoList }

    | StartEditingTodo todoId ->
        let nextEditModel =
            state.TodoList
            |> List.tryFind (fun todo -> todo.Id = todoId)
            |> Option.map (fun todo ->
                { Id = todo.Id
                  Description = todo.Description })

        { state with
              TodoBeingEdited = nextEditModel }

    | CancelEdit -> { state with TodoBeingEdited = None }

    | ApplyEdit ->
        match state.TodoBeingEdited with
        | None -> state
        | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
        | Some todoBeingEdited ->
            let nextTodoList =
                state.TodoList
                |> List.map (fun todo ->
                    if todo.Id = todoBeingEdited.Id then
                        { todo with
                              Description = todoBeingEdited.Description }
                    else
                        todo)

            { state with
                  TodoList = nextTodoList
                  TodoBeingEdited = None }

    | SetEditedDescription newText ->
        let nextEditModel =
            state.TodoBeingEdited
            |> Option.map (fun edit -> { edit with Description = newText })

        { state with
              TodoBeingEdited = nextEditModel }

    | DeleteTodo todoId ->
        let nextTodoList =
            state.TodoList
            |> List.filter (fun todo -> todo.Id <> todoId)

        { state with TodoList = nextTodoList }

let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div
        [ prop.classes classes
          prop.children children ]

let appTitle =
    Html.p
        [ prop.classes [ Tw.font_bold; Tw.text_xl; Tw.mb_2 ]
          prop.text "Elmish To-Do List" ]

let inputField (state: State) (dispatch: Msg -> unit) =
    div [ Tw.w_full; Tw.max_w_sm ]
        [ div
            [ Tw.flex
              Tw.items_center
              Tw.border_b
              Tw.border_b_2
              Tw.border_indigo_500
              Tw.py_2 ]
              [ Html.input
                  [ prop.classes editInputStyle
                    prop.valueOrDefault state.NewTodo
                    prop.onChange (SetNewTodo >> dispatch) ]

                Html.span
                    [ prop.classes
                        [ Tw.sm_ml_3
                          Tw.shadow_sm
                          Tw.rounded_md ]
                      prop.children
                          [ Html.button
                              [ prop.classes <| btn (Normal "indigo")
                                prop.onClick (fun _ -> dispatch AddNewTodo)
                                prop.children
                                    [ Html.svg
                                        [ prop.classes [ Tw._ml_1; Tw.mr_2; Tw.h_5; Tw.w_5 ]
                                          prop.fill "currentColor"
                                          prop.custom ("viewBox", "0 0 20 20")
                                          prop.children
                                              [ Html.path
                                                  [ prop.custom ("fill-rule", "evenodd")
                                                    prop.custom
                                                        ("d",
                                                         "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z")
                                                    prop.custom ("clip-rule", "evenodd") ] ] ]
                                      Html.text "Add" ] ] ] ] ] ]


let renderEditTodo (todoBeingEdited: TodoBeingEdited) (dispatch: Msg -> unit) =
    Html.li
        [ prop.children
            [ div [ Tw.px_4; Tw.py_4; Tw.sm_px_6 ]
                  [ div
                      [ Tw.flex
                        Tw.items_center
                        Tw.justify_between ]
                        [ Html.input
                            [ prop.classes editInputStyle
                              prop.valueOrDefault todoBeingEdited.Description
                              prop.onTextChange (SetEditedDescription >> dispatch) ]

                          div [ Tw.sm_ml_3; Tw.w_20 ]
                              [ Html.button
                                  [ prop.classes <| btn (Small "indigo")
                                    prop.onClick (fun _ -> dispatch ApplyEdit)
                                    prop.children [ Html.i [ prop.classes [ Fa.fa; Fa.fa_save; Tw.m_auto ] ] ] ]

                                Html.button
                                    [ prop.classes <| btn (Small "red")
                                      prop.onClick (fun _ -> dispatch CancelEdit)
                                      prop.children [ Html.i [ prop.classes [ Fa.fa; Fa.fa_arrow_right; Tw.m_auto ] ] ] ] ] ] ] ] ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
    Html.li
        [ prop.children
            [ div [ Tw.px_4; Tw.py_4; Tw.sm_px_6 ]
                  [ div
                      [ Tw.flex
                        Tw.items_center
                        Tw.justify_between ]
                        [ Html.p [ prop.text todo.Description ]

                          div []
                              [ Html.button
                                  [ prop.classes
                                    <| btn (Small(if todo.Completed then "green" else "indigo"))
                                    prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
                                    prop.children [ Html.i [ prop.classes [ Fa.fa; Fa.fa_check; Tw.m_auto ] ] ] ]

                                Html.button
                                    [ prop.classes <| btn (Small "indigo")
                                      prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id))
                                      prop.children [ Html.i [ prop.classes [ Fa.fa; Fa.fa_edit; Tw.m_auto ] ] ] ]

                                Html.button
                                    [ prop.classes <| btn (Small "red")
                                      prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
                                      prop.children [ Html.i [ prop.classes [ Fa.fa; Fa.fa_times; Tw.m_auto ] ] ] ] ] ] ] ] ]

let todoList (state: State) (dispatch: Msg -> unit) =
    Html.ul
        [ prop.classes [ Tw.w_full; Tw.max_w_sm ]
          prop.children
              [ for todo in state.TodoList ->
                  match state.TodoBeingEdited with
                  | Some todoBeingEdited when todoBeingEdited.Id = todo.Id -> renderEditTodo todoBeingEdited dispatch
                  | _ -> renderTodo todo dispatch ] ]


let render (state: State) (dispatch: Msg -> unit) =
    Html.div
        [ prop.classes [ Tw.p_5 ]
          prop.children
              [ appTitle
                inputField state dispatch
                todoList state dispatch ] ]

Stylesheet.apply "./styles/main.scss"

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
