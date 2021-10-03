// fsharplint:disable
module internal MobX

open Fable.Core.JsInterop
open Fable.Core

type Annotation =
    interface end

type [<AllowNullLiteral>] AnnotationMap =
    [<EmitIndexer>] abstract Item: field: string -> Annotation with get, set

type IObservableFactory =
    abstract ref: Annotation
    abstract ``struct``: Annotation

let observable: IObservableFactory = importMember "mobx"

let makeObservable<'T>(target: 'T, annotations: AnnotationMap) : 'T = importMember "mobx"
let makeAutoObservable<'T>(target: 'T, annotations: AnnotationMap) : 'T = importMember "mobx"

let action<'T>(name: string, fn: unit -> 'T): unit -> 'T = importMember "mobx"
let runInAction<'T>(fn: unit -> 'T): 'T = importMember "mobx"

type IDiposeSubscription = unit -> unit

type ISubscriptionOptions =
    abstract name: string

type IRepeatingSubscriptionOptions =
    abstract delay: int

type IReactionOptions =
    inherit ISubscriptionOptions
    inherit IRepeatingSubscriptionOptions
    abstract fireImmediately: bool

let autorun(fn: unit -> unit) : IDiposeSubscription = importMember "mobx"
let autorunOpt(fn: unit -> unit, options: IRepeatingSubscriptionOptions) : IDiposeSubscription = import "autorun" "mobx"
let reaction(data: unit -> 'a, effect: 'a -> unit) : IDiposeSubscription = importMember "mobx"
let reactionOpt(data: unit -> 'a, effect: 'a -> unit, options: IReactionOptions) : IDiposeSubscription = import "reaction" "mobx"

type Computation<'T> =
    abstract get: unit -> 'T

let computed<'T>(fn: unit -> 'T): Computation<'T> = importMember "mobx"
