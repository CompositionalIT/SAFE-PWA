module App

open Elmish
open Elmish.React
open Browser

//-:cnd:noEmit
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

promise {
    match Navigator.navigator.serviceWorker with
    | Some sw ->
        let! _ = sw.register ServiceWorker.exportPath
        ()
    | None ->
        ()

} |> Promise.start

//+:cnd:noEmit
Program.mkProgram Index.init Index.update Index.view
//-:cnd:noEmit
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
//+:cnd:noEmit
|> Program.run