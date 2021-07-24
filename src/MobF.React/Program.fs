module MobF.React

open Fable.Core.JsInterop
open Fable.React

type ViewFunction<'T> = 'T -> ReactElement

let inline observer<'T> (c: ViewFunction<'T>) = import<ViewFunction<'T>> "observer" "mobx-react-lite"
