module Index

open Elmish
open Shared
open Api
open DataAccess

let clearAllData _ =
    ServiceWorker.unregisterAllServiceWorkers ()
    DataAccess.destroyDatabase ()

type Model = { Todos: Todo list; Input: string }

type Msg =
    | GotTodos of Todo array
    | SetInput of string
    | AddTodo
    | AddedTodo of bool
    | UploadTodos
    | UploadedTodos of unit

let init () : Model * Cmd<Msg> =
    let model = { Todos = List.empty; Input = "" }
    let cmd = Cmd.OfPromise.perform Todos.findTodos () GotTodos
    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos ->
        { model with Todos = List.ofArray todos }, Cmd.none
    | SetInput value ->
        { model with Input = value }, Cmd.none
    | AddTodo ->
        if Todo.isValid model.Input then
            let todo = Todo.create model.Input
            let model = { model with Input = ""; Todos = todo::model.Todos }
            let cmd = Cmd.OfPromise.perform (Todos.putTodo todo) None AddedTodo
            model, cmd
        else
            // Probably want to show an error if invalid
            model, Cmd.none
    | AddedTodo succeeded ->
        // Probably want to show an error if it fails
        model, Cmd.none
    | UploadTodos ->
        model, Cmd.OfAsync.perform todosApi.uploadTodos model.Todos UploadedTodos
    | UploadedTodos () -> model, Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive

            // Important! Clear personal data from db when logging out
            prop.onClick clearAllData

            prop.text "Logout"
        ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li [ prop.text todo.Description ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "What needs to be done?"
                            prop.onChange (fun x -> SetInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "SAFE.App"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]