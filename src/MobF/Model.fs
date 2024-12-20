﻿namespace MobF

open System
open System.Collections.Generic
open FSharp.Reflection

open Fable.Core
open Fable.Core.JsInterop

open MobF
open MobF.Subscribe

type internal IModelStorage<'T> =
    abstract Read: 'T
    abstract Write: 'T -> unit

type internal ObservableBox<'T>(content: 'T) as this =
    do this.Content <- content

    [<DefaultValue>]
    val mutable Content: 'T

/// A placeholder interface for models which have no computed values.
type IEmptyComputation = interface end

/// Marker interface for extendable computations.
type IExtendable<'T> = interface end

module Computation =
    let private JSObject = JS.Constructors.Object
    let private annotationsSymbol = emitJsExpr<obj> null "Symbol('annotations')"

    /// Defines an empty computation.
    let empty _ =
        { new IEmptyComputation }

    /// Applies structural change detection behaviour to the given computation.
    let structural (computation: 'T when 'T : not struct) =
        let annotations = createEmpty<MobX.AnnotationMap>

        let protoOf x = JSObject.getPrototypeOf(x)
        let isObjectProto x = (protoOf x) = null

        box computation
        |> Seq.unfold (fun p ->
            if isObjectProto p then None
            else Some (p, protoOf p)
        )
        |> Seq.collect JSObject.getOwnPropertyNames
        |> Seq.iter (fun name -> annotations.[name] <- MobX.computed.``struct``)

        (computation, annotationsSymbol) ||> Expando.set annotations
        computation

    let internal create computation =
        let annotations =
            match computation |> Expando.get annotationsSymbol with
            | None -> createEmpty<MobX.AnnotationMap>
            | Some x ->
                computation |> Expando.delete annotationsSymbol
                x

        MobX.makeAutoObservable(computation, annotations)

[<AutoOpen>]
module Extendable =
    module Internal =
        let internal stateSymbol = emitJsExpr<obj> null "Symbol('state')"
        let internal extensionsSymbol = emitJsExpr<obj> null "Symbol('extensions')"

        let extend (define: 'T -> 'E) (extensionType: Type) (computation: IExtendable<'T>) : 'E =
            let key = extensionType.FullName
            let extensions = (computation, extensionsSymbol) ||> Expando.getOrSet Dictionary<string, obj>

            match extensions.TryGetValue(key) with
            | true, value -> value :?> 'E
            | false, _ ->
                match (computation |> Expando.get stateSymbol) with
                | None -> failwith "This type does not support extension"
                | Some (state: IModelStorage<'T>) ->
                    let value = Computation.create (define state.Read)
                    extensions.Add(key, value)
                    value

    type IExtendable<'T> with
        /// Extend computation with additional definitions.
        member inline this.Extend(define: 'T -> 'E) : 'E =
            this |> Internal.extend define typeof<'E>

type Post<'m> = ('m -> unit)

type Init<'s, 'm> = (Post<'m> -> 's)
type Update<'s, 'm, 'd> = ('s * Post<'m> * 'd  -> 'm -> 's)
type Compute<'s, 'd> = ('s -> 'd)

/// Reactive state which accepts update messages and supports computed properties.
type Model<'s, 'm, 'd> (init: Init<'s, 'm>, update: Update<'s, 'm, 'd>, compute: Compute<'s, 'd>, stateType: Type) as this =
    let t = stateType

#if DEBUG
    let ns = t.Namespace
#else
    let ns = ""
#endif

    let queue = ResizeArray<'m>()
    let mutable ready = false

    let subject = new ModelSubject<'s>((fun () -> this.State), ns)
    let withSubscribable = fun f -> f (subject :> ISubscribable<'s>)

    let initialState = MobX.runInAction(fun () ->
        let post m =
            if ready then this.Post m
            else queue.Add m

        subject |> Subscribe.capture (fun () ->
            init post
        )
    )

    let computed = Computation.create (compute initialState)

    let storage =
        if t = typeof<Unit> then 
            { new IModelStorage<_> with
                member _.Read = unbox ()
                member _.Write(_) = ()
            }

        elif FSharpType.IsRecord(t) then
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

        if queue.Count > 0 then
            let initMessages = List.ofSeq queue
            queue.Clear()
            initMessages |> Seq.iter this.Post

        (computed, Extendable.Internal.stateSymbol) ||> Expando.set storage
        ready <- true

    /// The current state.
    member _.State = storage.Read

    /// The computed values.
    member _.Computed = computed

    /// <summary>
    ///     Apply the message to the current state using the update function.
    /// </summary>
    /// <remarks>
    ///     The update function is executed in a MobX action, allowing you to
    ///     update other models in a transactional fashion.
    /// </remarks>
    member this.Post msg =
        queue.Add(msg)

        //assumption: because this is running in a JavaScript environment with
        //single-thread semantics, we can model a message queue by permitting
        //only the 'top level' invocation to dequeue messages.
        if queue.Count = 1 then
            while queue.Count > 0 do
                let msg = queue.[0]

                let update () =
                    subject |> Subscribe.capture (fun () ->
                        let currentState = storage.Read
                        let nextState = update (currentState, this.Post, this.Computed) msg
                        storage.Write nextState
                    )

#if DEBUG
                MobX.action($"post [%s{ns}] %A{msg}", update)()
#else
                MobX.runInAction(update)
#endif

                queue.RemoveAt(0)

    interface ISubscribable<'s> with
        member _.IsActive =
            withSubscribable <| fun x -> x.IsActive

        member _.AutoSubscribe(subscriber, select, effect, triggerImmediately) =
            withSubscribable <| fun x -> x.AutoSubscribe(subscriber, select, effect, triggerImmediately)

        member _.Subscribe(select, effect, triggerImmediately) =
            withSubscribable <| fun x -> x.Subscribe(select, effect, triggerImmediately)

    interface IDisposable with
        member _.Dispose() =
            (subject :> IDisposable).Dispose()

    override _.ToString() = sprintf "%A" storage.Read

/// Reactive state which accepts update messages.
type Model<'s, 'm>(init: Init<'s, 'm>, update: Update<'s, 'm, IEmptyComputation>, stateType: Type) =
    inherit Model<'s, 'm, IEmptyComputation>(init, update, Computation.empty, stateType)

[<RequireQualifiedAccess>]
module Model =
    /// Use a function to create initial model state.
    let inline useInit init : Init<'s, 'm> =
        fun _ -> init ()

    /// Use a function which can post messages to create initial model state.
    let inline useInitWithPost init : Init<'s, 'm> =
        init

    /// Use a function to update model state.
    let inline andUpdate update init : Init<'s, 'm> * Update<'s, 'm, 'd> =
        init, fun (state, _, _) msg -> update state msg

    /// Use a function which can post messages to update model state.
    let inline andUpdateWithPost update init : Init<'s, 'm> * Update<'s, 'm, 'd> =
        init, fun (state, post, _) msg -> update (state, post) msg

    /// Create a model with init and update functions.
    let inline create<'s, 'm, 'd> (init: Init<'s, 'm>, update: Update<'s, 'm, _>) =
        new Model<'s, 'm>(init, update, typeof<'s>)

    /// Use a function which can post messages and read computed values to
    /// update model state.
    let inline andUpdateWithComputed update init : Init<'s, 'm> * Update<'s, 'm, 'd> =
        init, update

    /// Create a model with init, update and compute functions.
    let inline createWithComputed<'s, 'm, 'd> compute (init: Init<'s, 'm>, update: Update<'s, 'm, 'd>) =
        new Model<'s, 'm, 'd>(init, update, compute, typeof<'s>)

    /// Dispose a model and any dependent operations like subscriptions
    let inline dispose (model: Model<_, _, _>) =
        (model :> IDisposable).Dispose()

module Reactive =
    let autorun = MobX.autorun

    let seq f =
        let computed = MobX.computed.Invoke(f)
        seq { yield! computed.get() }
