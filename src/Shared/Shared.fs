namespace Shared

open System

type Id =
    | Id of Guid
    member this.Value = match this with | Id id -> id

type Revision =
    | Revision of string
    member this.Value = match this with | Revision rev -> rev

type Query =
    | LessThan of string
    | GreaterThan of string
    | LessThanOrEqual of string
    | GreaterThanOrEqual of string
    | Equal of string
    | NotEqual of string
    | Exists of string
    | Type of string
    | In of string
    | And of string
    | NotIn of string
    | All of string
    | Size of string
    | Or of string
    | Nor of string
    | Not of string
    | Mod of string
    | Regex of string
    | ElemMatch of string

    static member toSelector = function
        | LessThan s -> "$lt", s
        | GreaterThan s -> "$gt", s
        | LessThanOrEqual s -> "$lte", s
        | GreaterThanOrEqual s -> "$gte", s
        | Equal s -> "$eq", s
        | NotEqual s -> "$ne", s
        | Exists s -> "$exists", s
        | Type s -> "$type", s
        | In s -> "$in", s
        | And s -> "$and", s
        | NotIn s -> "$nin", s
        | All s -> "$all", s
        | Size s -> "$size", s
        | Or s -> "$or", s
        | Nor s -> "$nor", s
        | Not s -> "$not", s
        | Mod s -> "$mod", s
        | Regex s -> "$regex", s
        | ElemMatch s -> "$elemMatch", s

type FindRequest =
    { Selector: Map<string, Query list>
      Sort: string list }
      
type Todo = { Id: Guid; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    { getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo> }