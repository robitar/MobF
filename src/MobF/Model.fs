namespace MobF

open System
open System.Collections.Generic

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

/// A placeholder interface for models which have no computed values.
type IEmptyComputation = interface end

module EmptyComputation =
    /// Defines an empty computation.
    let compute _ =
        { new IEmptyComputation }

type Post<'m> = ('m -> unit)

type Init<'s, 'm> = (Post<'m> -> 's)
type Update<'s, 'm, 'd> = ('s * Post<'m> * 'd  -> 'm -> 's)
type Compute<'s, 'd> = ('s -> 'd)

/// Reactive state which accepts update messages and supports computed properties.
type Model<'s, 'm, 'd> (init: Init<'s, 'm>, update: Update<'s, 'm, 'd>, compute: Compute<'s, 'd>, [<Inject>] ?resolver: ITypeResolver<'s>) as this =
    let t = resolver.Value.ResolveType()

#if DEBUG
    let ns = t.Namespace
#else
    let ns = ""
#endif

    let subscriptions = Dictionary<Guid, MobX.IDiposeSubscription>()
    let queue = ResizeArray<'m>()
    let mutable ready = false

    let initialState = MobX.runInAction(fun () ->
        let post m =
            if ready then this.Post m
            else queue.Add m
        init post
    )

    let computed = MobX.makeAutoObservable(compute initialState, createEmpty<MobX.AnnotationMap>)

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

        if queue.Count > 0 then
            let initMessages = List.ofSeq queue
            queue.Clear()
            initMessages |> Seq.iter this.Post

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
                    let currentState = storage.Read
                    let nextState = update (currentState, this.Post, this.Computed) msg
                    storage.Write nextState

#if DEBUG
                MobX.action($"[%s{ns}] %A{msg}", update)()
#else
                MobX.runInAction(update)
#endif

                queue.RemoveAt(0)

    /// <summary>
    ///     Register an effect which is triggered when selected state changes.
    /// </summary>
    /// <remarks>
    ///     The effect is only triggered when the selected value differs from
    ///     its previous state; in other words, successive updates which do not
    ///     effectively change the selected value will be ignored. By default,
    ///     effects are only triggered by changes which occur after the
    ///     subscription is registered, to trigger the effect immediately
    ///     with the current state, set <paramref name ="triggerImmediately" /> to
    ///     true.
    /// </remarks>
    /// <param name="select">Select a value from the state to observe</param>
    /// <param name="effect">The effect to trigger when the state changes</param>
    /// <param name="triggerImmediately">Trigger the effect when the subscription is registered</param>
    member this.Subscribe(select: 's -> 'a, effect: 'a -> unit, ?triggerImmediately: bool) =
        let id = Guid.NewGuid()

        let options = createEmpty<MobX.IReactionOptions>
        options.fireImmediately <- (triggerImmediately |> Option.defaultValue false)

#if DEBUG
        options.name <- sprintf "[%s] %A" ns id
#endif

        let dispose = MobX.reactionOpt((fun () -> select this.State), effect, options)
        subscriptions.Add(id, dispose)

        { new IDisposable with
            member _.Dispose () =
                match subscriptions.TryGetValue id with
                | true, dispose ->
                    dispose()
                    subscriptions.Remove(id) |> ignore
                | false, _ -> ()
        }

    override _.ToString() = sprintf "%A" storage.Read

    interface IDisposable with
        member _.Dispose() =
            for dispose in subscriptions.Values do dispose()
            subscriptions.Clear()

/// Reactive state which accepts update messages.
type Model<'s, 'm>(init: Init<'s, 'm>, update: Update<'s, 'm, IEmptyComputation>, [<Inject>] ?resolver: ITypeResolver<'s>) =
    inherit Model<'s, 'm, IEmptyComputation>(init, update, EmptyComputation.compute, ?resolver=resolver)

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
    let inline create (init, update) =
        new Model<_, _>(init, update)

    /// Use a function which can post messages and read computed values to
    /// update model state.
    let inline andUpdateWithComputed update init : Init<'s, 'm> * Update<'s, 'm, 'd> =
        init, update

    /// Create a model with init, update and compute functions.
    let inline createWithComputed compute (init, update) =
        new Model<_, _, _>(init, update, compute)

module Reaction =
    let autorun = MobX.autorun
