// fsharplint:disable
module internal MobX

open Fable.Core.JsInterop
open Fable.Core

type Computation<'T> =
    abstract get: unit -> 'T

type Annotation =
    interface end

type [<AllowNullLiteral>] AnnotationMap =
    [<EmitIndexer>] abstract Item: field: string -> Annotation with get, set

type IObservableFactory =
    abstract ref: Annotation
    abstract ``struct``: Annotation

let observable: IObservableFactory = importMember "mobx"

let makeObservable<'T>(target: 'T, annotations: AnnotationMap): 'T = importMember "mobx"
let makeAutoObservable<'T>(target: 'T, annotations: AnnotationMap): 'T = importMember "mobx"
let autorun(fn: unit -> unit) = importMember "mobx"
let runInAction<'T>(fn: unit -> 'T): 'T = importMember "mobx"
let computed<'T>(fn: unit -> 'T): Computation<'T> = importMember "mobx"
