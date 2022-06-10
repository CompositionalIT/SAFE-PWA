module ServiceWorker

open Fable.Core.JsInterop
open Browser.Types
open Browser
open Fable.Core
open Fetch

let exportPath = "/service-worker.js"

let unregisterAllServiceWorkers _ = 
    promise {
        match Navigator.navigator.serviceWorker with
        | Some sw ->
            let! registrations = sw.getRegistrations()
            for reg in registrations do
                let! _ = reg.unregister()
                ()
        | None ->
            ()
    }|> Promise.start

type [<AllowNullLiteral>] Cache =
    abstract add: request: Request -> JS.Promise<unit>
    abstract addAll: requestArray: Request array -> JS.Promise<unit>
    abstract delete: request: Request * ?options: CacheStorageOptions -> JS.Promise<bool>
    abstract keys: ?request: Request * ?options: CacheStorageOptions -> JS.Promise<string array>
    abstract ``match``: request: Request * ?options: CacheStorageOptions -> JS.Promise<Response>
    abstract matchAll: request: Request * ?options: CacheStorageOptions -> JS.Promise<Response array>
    abstract put: request: U2<Request, string> * response: Response -> JS.Promise<unit>

and [<AllowNullLiteral>] CacheStorageOptions =
    abstract cacheName: string option with get, set
    abstract ignoreMethod: bool option with get, set
    abstract ignoreSearch: bool option with get, set
    abstract ignoreVary: bool option with get, set

and [<AllowNullLiteral>] CacheStorage =
    abstract delete: cacheName: string -> JS.Promise<bool>
    abstract has: cacheName: string -> JS.Promise<bool>
    abstract keys: unit -> JS.Promise<string array>
    abstract ``match``: request: Request * ?options: CacheStorageOptions -> JS.Promise<Response>
    abstract ``open``: cacheName: string -> JS.Promise<Cache>

let CACHE_NAME = "SAFE-PWA-Cache-v4"

let resources = [|
    "app.js"
    "/Images/safe_favicon.png"
    "index.html"
    "manifest.json"
    "/"
    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.14.0/webfonts/fa-solid-900.woff"
    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.14.0/webfonts/fa-solid-900.woff2"
    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.14.0/webfonts/fa-solid-900.ttf"
    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.14.0/css/all.min.css"
    "https://cdn.jsdelivr.net/npm/bulma@0.9.0/css/bulma.min.css"
|]

let [<Global>] caches : CacheStorage = jsNative
let [<Global>] self : ServiceWorker = jsNative

self.addEventListener("install", (fun event ->
    console.log "Installing"
    promise {
        let! cache = caches.``open`` CACHE_NAME
        do! cache.addAll !!resources
    } |> Promise.start
))

self.addEventListener("activate", (fun event ->
    console.log "Activating"
    promise {
        let! keys = caches.keys()
        let promises : JS.Promise<bool> array =
            !!keys
            |> Array.map(fun k ->
                if k <> CACHE_NAME then
                    console.log "Removing old cache "
                    caches.delete(k)
                else promise.Return true)
        let! _ = Promise.all promises
        ()
    } |> Promise.start
))

self.addEventListener("fetch", (fun event ->
    let req = event?request
    let urlString : string = req?url
    let url = req?url |> URL.Create
    // check if request is made by chrome extensions or web page
    // if request is made for web page url must contains http.
    if not (urlString.Contains("http")) then
        () // skip the request. if request is not made with http protocol
    else
        // Don't handle login / logout paths (original request or Active Directory redirects etc) 
        if urlString.Contains("logout") then
            // Clear cache if this is a logout request
            if url.pathname.StartsWith("/logout") then
                promise {
                    let! keys = caches.keys()
                    let promises : JS.Promise<bool> array =
                        !!keys
                        |> Array.map(fun k ->
                            console.log "Removing all caches "
                            caches.delete(k))
                    let! _ = Promise.all promises
                    ()
                } |> Promise.start
            ()
        else
            console.log("Fetching")
            console.log($"METHOD: {event?request?method} and URL: {url}");
            event?respondWith(promise {
                let! res = caches.``match``(req)
                console.log("Request: ", req);
                if isNullOrUndefined res then
                    let! resp = fetch req.url []
                    if not <| url.pathname.StartsWith("/api/") then
                        console.log($"Adding {req} to the cache")
                        let! cache = caches.``open`` CACHE_NAME
                        do! cache.put(!!req, !!(resp?clone()))
                    return !!resp
                else
                    return res
    })))
