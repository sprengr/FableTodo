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

    | DeleteTodo todoId ->
        let nextTodoList =
            state.TodoList
            |> List.filter (fun todo -> todo.Id <> todoId)

        { state with TodoList = nextTodoList }

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

let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div
        [ prop.classes classes
          prop.children children ]

let appTitle =
    Html.p
        [ prop.classes [ Tw.FontBold; Tw.TextXl; Tw.Mb2]
          prop.text "Elmish To-Do List" ]

let inputField (state: State) (dispatch: Msg -> unit) =
    div [ Tw.WFull; Tw.MaxWSm ]
        [ div [ Tw.Flex; Tw.ItemsCenter; Tw.BorderB; Tw.BorderB2; Tw.BorderIndigo500; Tw.Py2 ]
              [ Html.input
                  [ prop.className
                      "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                    prop.valueOrDefault state.NewTodo
                    prop.onChange (SetNewTodo >> dispatch) ]

                Html.span
                    [ prop.className "sm:ml-3 shadow-sm rounded-md"
                      prop.children
                          [ Html.button
                              [ prop.className
                                  "inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-500 focus:outline-none focus:shadow-outline-indigo focus:border-indigo-700 active:bg-indigo-700 transition duration-150 ease-in-out"
                                prop.onClick (fun _ -> dispatch AddNewTodo)
                                prop.children
                                    [ Html.svg
                                        [ prop.className "-ml-1 mr-2 h-5 w-5"
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

let btnStyle = [ "w-6 inline-flex items-center mr-1 px-1 py-1 text-xs font-thin border border-transparent rounded-md text-white focus:outline-none focus:shadow-outline-indigo transition duration-150 ease-in-out" ]
let btnStyleColored color =
    List.append btnStyle [ sprintf "bg-"+color+"-600 hover:bg-"+color+"-500 focus:border-"+color+"-700 active:bg-"+color+"-700" ]

let renderEditTodo (todoBeingEdited: TodoBeingEdited) (dispatch: Msg -> unit) =
    Html.li
        [ prop.children
            [ div [ Tw.Px4; Tw.Py4; Tw.SmPx6 ]
                  [ div [ Tw.Flex; Tw.ItemsCenter; Tw.JustifyBetween ]
                        [ Html.input
                            [ prop.className "subtitle"
                              prop.valueOrDefault todoBeingEdited.Description
                              prop.onTextChange (SetEditedDescription >> dispatch) ]

                          div [ "buttons" ]
                              [ Html.button
                                  [ prop.classes <| btnStyleColored "indigo"
                                    prop.onClick (fun _ -> dispatch ApplyEdit)
                                    prop.children [ Html.i [ prop.classes [ "fa"; "fa-save"; "m-auto" ] ] ] ]

                                Html.button
                                    [ prop.classes <| btnStyleColored "red"
                                      prop.onClick (fun _ -> dispatch CancelEdit)
                                      prop.children [ Html.i [ prop.classes [ "fa"; "fa-arrow-right"; "m-auto" ] ] ] ] ] ] ] ] ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
    Html.li
        [ prop.children
            [ div [ "px-4 py-4 sm:px-6" ]
                  [ div [ "flex items-center justify-between" ]
                        [ Html.p
                            [ prop.className "subtitle"
                              prop.text todo.Description ]

                          div [ "buttons" ]
                              [ Html.button
                                  [ prop.classes
                                      [ "w-6 inline-flex items-center mr-1 px-1 py-1 text-xs font-thin border border-transparent rounded-md text-white focus:outline-none focus:shadow-outline-indigo transition duration-150 ease-in-out"
                                        if todo.Completed then
                                            "bg-green-600 hover:bg-green-500 focus:border-green-700 active:bg-green-700"
                                        else
                                            "bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 active:bg-indigo-700" ]
                                    prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
                                    prop.children [ Html.i [ prop.classes [ "fa"; "fa-check"; "m-auto" ] ] ] ]

                                Html.button
                                    [ prop.classes <| btnStyleColored "indigo"
                                      prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id))
                                      prop.children [ Html.i [ prop.classes [ "fa"; "fa-edit"; "m-auto" ] ] ] ]

                                Html.button
                                    [ prop.classes <| btnStyleColored "red"
                                      prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
                                      prop.children [ Html.i [ prop.classes [ "fa"; "fa-times"; "m-auto" ] ] ] ] ] ] ] ] ]

let todoList (state: State) (dispatch: Msg -> unit) =
    Html.ul
        [ prop.className "w-full max-w-sm"
          prop.children
              [ for todo in state.TodoList ->
                  match state.TodoBeingEdited with
                  | Some todoBeingEdited when todoBeingEdited.Id = todo.Id -> renderEditTodo todoBeingEdited dispatch
                  | _ -> renderTodo todo dispatch ] ]


let render (state: State) (dispatch: Msg -> unit) =
    Html.div
        [ prop.style [ style.padding 20 ]
          prop.children
              [ appTitle
                inputField state dispatch
                todoList state dispatch ] ]

Stylesheet.apply "./styles/main.scss"

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
