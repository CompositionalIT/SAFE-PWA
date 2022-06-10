module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module Storage =
    let savedTodos = ResizeArray()
    
    let addTodos (todos: Todo seq) =
        if todos |> Seq.forall (fun todo -> Todo.isValid todo.Description) then
            savedTodos.AddRange todos
            printfn $"*** Uploaded Todos ***\n%A{todos}"
            Ok()
        else
            Error "Invalid todo found"

let todosApi =
    { uploadTodos =
        fun todos ->
            async {
                return
                    match Storage.addTodos todos with
                    | Ok () -> ()
                    | Error e -> failwith e
            } }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue todosApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://*:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0