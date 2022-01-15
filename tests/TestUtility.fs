[<AutoOpen>]
module Tests.TestUtility

open System
open MobF

open Fable.Mocha
open Fable.React
open Fable.Core.JsInterop
open Browser.Types

let expectThrows actual =
    try
        actual()
        Expect.equal true false "Expected an exception but none was thrown"
    with _ ->
        Expect.pass()

let Should = "Assertion failed"

type ICounter =
    abstract Value: int

type IDisposableCounter =
    inherit IDisposable
    inherit ICounter

let countReactions f =
    let mutable value = -1
    Reaction.autorun (fun () -> f() |> ignore; value <- value + 1) |> ignore
    { new ICounter with member _.Value = value }

module GlobalJsDom =
    let cleanup() = importDefault<unit> "global-jsdom/esm/index"

// fsharplint:disable

module ReactTestingLibrary =
    type IRenderResult =
        abstract container: HTMLElement
        abstract getByTestId: string -> HTMLElement
        abstract rerender: ReactElement -> unit

    let render(element: ReactElement) = importMember<IRenderResult> "@testing-library/react"
    let cleanup() = importMember<unit> "@testing-library/react"
