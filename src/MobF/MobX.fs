module internal MobX

open Fable.Core.JsInterop
open Fable.Core

type IComputation<'T> =
    abstract get: unit -> 'T

type IAnnotation =
    interface end

type [<AllowNullLiteral>] AnnotationMap =
    [<EmitIndexer>] abstract Item: field: string -> IAnnotation with get, set

type IObservableFactory =
    abstract ref: IAnnotation
    abstract ``struct``: IAnnotation

let observable: IObservableFactory = importMember "mobx"

let makeObservable<'T>(target: 'T, annotations: AnnotationMap): 'T = importMember "mobx"
let autorun(fn: unit -> unit) = importMember "mobx"
let runInAction<'T>(fn: unit -> 'T): 'T = importMember "mobx"
let computed<'T>(fn: unit -> 'T): IComputation<'T> = importMember "mobx"
