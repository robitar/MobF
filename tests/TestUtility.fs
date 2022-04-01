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
    Reactive.autorun (fun () -> f() |> ignore; value <- value + 1) |> ignore
    { new ICounter with member _.Value = value }

module Note =
    type State = {
        Text: string
    }

    type Msg =
        | Edit of string

    type IComputed =
        inherit IExtendable<State>
        abstract IsEmpty: bool

    type Type = Model<State, Msg, IComputed>

    let create text =
        let init () =
            { Text = text }

        let update state = function
            | Edit text -> { state with Text = text }

        let compute state =
            { new IComputed with
                member _.IsEmpty =
                    match state.Text with
                    | "" -> true
                    | _ -> false
            }

        Model.useInit init
        |> Model.andUpdate update
        |> Model.createWithComputed compute

module GlobalJsDom =
    let cleanup() = importDefault<unit> "global-jsdom"

// fsharplint:disable

module ReactTestingLibrary =
    type IRenderResult =
        abstract container: HTMLElement
        abstract getByTestId: string -> HTMLElement
        abstract rerender: ReactElement -> unit

    let render(element: ReactElement) = importMember<IRenderResult> "@testing-library/react"
    let cleanup() = importMember<unit> "@testing-library/react"
