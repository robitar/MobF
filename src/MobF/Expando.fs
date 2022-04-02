module internal MobF.Expando

open Fable.Core.JsInterop

let inline set value instance key =
    emitJsStatement (instance, key, value) "$0[$1] = $2" |> ignore

let inline get key instance =
    emitJsExpr<_ option> (instance, key) "$0[$1]"

let inline delete key instance =
    emitJsStatement<_ option> (instance, key) "delete $0[$1]" |> ignore

let inline getOrSet factory instance key =
    match (instance |> get key) with
    | Some value -> value
    | None ->
        let value = factory ()
        (instance, key) ||> set value
        value
