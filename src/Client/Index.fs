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
    | UploadedTodos of Todo list
    | UploadFailed of exn
    | TodosUpdated of Todo array

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
            printfn "Invalid todo"
            model, Cmd.none
    | AddedTodo succeeded ->
        printfn "Failed to add todo"
        model, Cmd.none
    | UploadTodos ->
        let notUploadedTodos =
            model.Todos
            |> List.filter (fun todo -> not todo.Uploaded)
        model, Cmd.OfAsync.either todosApi.uploadTodos notUploadedTodos (fun () -> UploadedTodos notUploadedTodos) UploadFailed
    | UploadedTodos uploadedTodos ->
        model, Cmd.OfPromise.perform Todos.setTodosUploaded (Array.ofList uploadedTodos) TodosUpdated
    | TodosUpdated updatedTodos ->
        let todos =
            model.Todos
            |> List.map (fun todo ->
                match updatedTodos |> Array.tryFind (fun r -> r.Id = todo.Id) with
                | Some newTodo -> newTodo
                | None -> todo)
        { model with Todos = todos }, Cmd.none
    | UploadFailed exn->
        printfn $"Failed to upload todos. Are you offline? Exception message:{exn.Message} "
        model, Cmd.none

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
                        prop.text "Save"
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isInfo
                        prop.onClick (fun _ -> dispatch UploadTodos)
                        prop.text "Upload"
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