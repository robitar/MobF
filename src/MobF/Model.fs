namespace MobF

open System.Runtime.CompilerServices

open FSharp.Reflection

open Fable.Core
open Fable.Core.JsInterop

type internal IModelStorage<'T> =
    abstract Read: 'T
    abstract Write: 'T -> unit

type internal ObservableBox<'T>(content: 'T) as this =
    do this.Content <- content

    [<DefaultValue>]
    val mutable Content: 'T

/// Reactive state which accepts update messages and supports computed views.
type Model<'s, 'm> (init: unit -> 's, update: 's -> 'm -> 's, [<Inject>] ?resolver: ITypeResolver<'s>) as this =
    let t = resolver.Value.ResolveType()
    let initialState = MobX.runInAction(init)

    let storage =
        if FSharpType.IsRecord(t) then 
            let annotations =
                (createEmpty<MobX.AnnotationMap>, FSharpType.GetRecordFields(t)) 
                ||> Array.fold (fun a x ->
                    a.[x.Name] <- MobX.observable.ref; a
                )

            let state = MobX.makeObservable(initialState, annotations)

            { new IModelStorage<_> with
                member _.Read = state
                member _.Write x = JS.Constructors.Object.assign(state, x) |> ignore
            }

        else if FSharpType.IsUnion(t) then
            let box = ObservableBox(initialState)

            let annotations = createEmpty<MobX.AnnotationMap>
            annotations.[nameof box.Content] <- MobX.observable.``struct``
            
            let box = MobX.makeObservable(box, annotations)
            
            { new IModelStorage<_> with
                member _.Read = box.Content
                member _.Write x = box.Content <- x
            }
        else
            failwith "State must be a record or union type"

    do
        let debugView () = this.ToString()
        emitJsStatement (this, debugView) "Object.defineProperty($0, 'debugView', { enumerable: true, get: $1 })"

    /// The current state.
    member _.State = storage.Read

    /// <summary>
    ///     Apply the message to the current state using the update function.
    /// </summary>
    /// <remarks>
    ///     The update function is executed in a MobX action, allowing you to
    ///     update other models in a transactional fashion.
    /// </remarks>
    member _.Post msg =
        MobX.runInAction(fun () ->
            let currentState = storage.Read
            let nextState = update currentState msg
            storage.Write nextState
        )

    /// <summary>
    ///     Evaluate a computed view
    /// </summary>
    /// <remarks>
    ///     On first use, the definition is boxed into a MobX computed
    ///     observable, and cached on the model with the supplied name as key.
    ///     Subsequent invocations then evaluete this observable.
    /// </remarks>
    member model.Computed(definition: 's -> 'a, [<CallerMemberName>] ?name: string) =
        match name with
        | None -> failwith "A name must be provided for the computed definition"
        | Some name ->
            let key = "_" + name
            let c: MobX.IComputation<'a> option = emitJsExpr (model, key) "$0[$1]"

            match c with
            | Some c -> c.get()
            | None ->
                let c = MobX.computed(fun () -> definition model.State)
                emitJsStatement (model, key, c) "$0[$1] = $2"
                c.get()

    override _.ToString() = sprintf "%A" storage.Read

module Reaction =
    let autorun = MobX.autorun
