module DataAccess

open Fable.Core
open Fable.Core.JsInterop
open Shared

type OpenRevisions =
    | All
    | Specific of Revision array

type AttachmentType =
    | Binary
    | Base64

type PutOptions =
    { Force : bool }

type GetOptions =
    { SpecificRevision : Revision option
      IncludeRevisionHistory : bool option
      IncludeRevisionInfo : bool option
      FetchLeafRevisions : OpenRevisions option
      AttachConflicts : bool option
      IncludeAttachments : AttachmentType option
      RetrieveLatest : bool option}

type DbInfo =
    { Name : string
      DocCount: int
      UpdateSeq : int }

type DbPutOptions =
    {| force : bool |}

type Response =
    {| ok: bool
       id: string
       rev: string |}

type DbGetOptions =
    {| rev : string |}

type DbInfoResponse =
    {| db_name : string
       doc_count: int
       update_seq : int |}

type DbFindRequest =
    {| selector: obj
       fields: string array
       sort: string array option |}

type DbFindResponse =
    {| docs: obj array |}

type DocId = string

type PouchDb =
    abstract info : unit -> JS.Promise<DbInfoResponse>
    abstract put : obj -> JS.Promise<Response>
    abstract put : obj * DbPutOptions -> JS.Promise<Response>
    abstract bulkDocs : obj array -> JS.Promise<Response array>
    abstract get : DocId * DbGetOptions -> JS.Promise<obj>
    abstract get : DocId -> JS.Promise<obj>
    abstract remove : DocId * string -> JS.Promise<Response>
    abstract destroy : unit -> JS.Promise<unit>
    abstract find : DbFindRequest ->  JS.Promise<DbFindResponse>

let pouchDbFind: obj = importDefault "pouchdb-find"

let pouchdb name : PouchDb =
    let pdb = importDefault "pouchdb"
    pdb?plugin(pouchDbFind)
    pdb name

let dbName = "SAFEPWADB"
let private db = pouchdb dbName

let destroyDatabase () =
    db.destroy () |> Promise.start

let getDatabaseInfo () = promise {
        let! info = db.info()
        return
            { Name = info.db_name
              DocCount = info.doc_count
              UpdateSeq = info.update_seq }
    }

module Todos =

    let findTodos () =
        let request =
            { Selector = Map ["documentType", [ Equal "Todo" ]]; Sort = [] }

        {| selector =
            request.Selector
            |> Map.toArray
            |> Array.map(fun (propName, queries) ->
                let queryObject =
                    queries
                    |> List.map Query.toSelector
                    |> List.map (fun (selector, value) -> selector, box value)
                    |> List.toArray
                    |> createObj
                propName, queryObject)
            |> createObj

           fields = Todo.getFields |> List.toArray
           sort =
                if request.Sort |> List.isEmpty then
                    None
                else request.Sort |> List.toArray |> Some |}
        |> db.find
        |> Promise.map (fun response ->
            response.docs
            |> Array.map Todo.Deserialise)

    let putTodo (todo: Todo) opts =
        match opts with
        | Some o -> db.put (todo.Serialise (), o)
        | None -> db.put (todo.Serialise ())
        |> Promise.map (fun res -> res.ok)