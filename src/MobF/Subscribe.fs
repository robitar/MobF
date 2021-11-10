module MobF.Subscribe

open System

type IAutoObserver =
    abstract Description: string
    abstract Add: subscription: IAutoSubscription -> unit
    abstract Remove: subscription: IAutoSubscription -> unit

and IAutoSubscription =
    abstract Id: Guid
    abstract Detach: notifier: IAutoObserver -> unit

type ISubscribable<'T> =
    abstract IsActive: bool
    abstract AutoSubscribe: observer: IAutoObserver * select: ('T -> 'a) * effect: ('a -> unit) * triggerImmediately: bool -> Unit
    abstract Subscribe: select: ('T -> 'a) * effect: ('a -> unit) * triggerImmediately: bool -> IDisposable

let mutable private currentObservers = List.empty

/// Register an automatic subscription observer for the duration of the given function
let capture f (observer: IAutoObserver) =
    try
        currentObservers <- observer :: currentObservers
        f()
    finally
        currentObservers <- currentObservers.Tail

let private autoImpl (select: 'T -> 'a) (effect: 'a -> unit) (triggerImmediately: bool) (source: ISubscribable<'T>) : unit =
    match currentObservers with
    | [] -> failwith "There is no observer capturing auto subscriptions"
    | observer :: _ -> source.AutoSubscribe(observer, select, effect, triggerImmediately)

/// Establish an automatic subscription with immediate effect
let autoImmediate select effect source =
    source |> autoImpl select effect true

/// Establish an automatic subscription
let auto select effect source =
    source |> autoImpl select effect false

let private manualImpl (select: 'T -> 'a) (effect: 'a -> unit) (triggerImmediately: bool) (source: ISubscribable<'T>) =
    source.Subscribe(select, effect, triggerImmediately)

/// Create a manual subscription
let manual select effect source =
    source |> manualImpl select effect false

/// Create a manual subscription with immediate effect
let manualImmediate select effect source =
    source |> manualImpl select effect true

open Fable.Core.JsInterop
open System.Collections.Generic

type internal ModelSubject<'T>(state: unit -> 'T, description) as this =
    let mutable cleanupActions = None

    let useCleanupActions f =
        match cleanupActions with
        | Some x -> f x
        | None ->
            let x = Dictionary<Guid, unit -> unit>()
            cleanupActions <- Some x
            f x

    let addAutoCleanup (sub: IAutoSubscription) =
        useCleanupActions <| fun actions ->
            let action () = sub.Detach(this)
            actions.Add(sub.Id, action)

    let removeCleanup id =
        useCleanupActions <| fun actions ->
            actions.Remove(id)

    let makeReaction select effect triggerImmediately name =
        let id = Guid.NewGuid()

        let options = createEmpty<MobX.IReactionOptions>
        options.fireImmediately <- triggerImmediately
        options.name <- name

        let dispose = MobX.reactionOpt((fun () -> select (state())), effect, options)
        id, dispose

    interface ISubscribable<'T> with
        member _.IsActive =
            match cleanupActions with
            | None -> false
            | Some actions -> actions.Count > 0

        member _.AutoSubscribe(observer, select, effect, triggerImmediately) =
            let name = sprintf "sub [%s] -> [%s]" observer.Description description
            let id, dispose = makeReaction select effect triggerImmediately name

            let rec sub =
                { new IAutoSubscription with
                    member _.Id = id
                    member _.Detach(notifier) =
                        let notifierIs = LanguagePrimitives.PhysicalEquality notifier

                        if notifierIs this then observer.Remove(sub)
                        elif notifierIs observer then removeCleanup id |> ignore
                        else failwithf "The notifier is not a party to this subscription: %A" notifier

                        dispose()
                }

            addAutoCleanup sub
            observer.Add(sub)

        member _.Subscribe(select, effect, triggerImmediately) =
            let name = sprintf "sub [%s]" description
            let id, dispose = makeReaction select effect triggerImmediately name

            useCleanupActions <| fun actions ->
                actions.Add(id, dispose)

            { new IDisposable with
                member _.Dispose() = if removeCleanup id then dispose()
            }

    interface IAutoObserver with
        member _.Description = description
        member _.Add(subscription) = addAutoCleanup subscription
        member _.Remove(subscription) = removeCleanup subscription.Id |> ignore

    interface IDisposable with
        member _.Dispose() =
            match cleanupActions with
            | None -> ()
            | Some actions -> for a in actions.Values do a()
            cleanupActions <- None
