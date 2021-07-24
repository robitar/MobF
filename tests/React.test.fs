module Tests.React

open Fable.React
open Fable.React.ReactBindings
open Fable.Mocha
open Fable.Core.JsInterop

open MobF
open MobF.React

importSideEffects "global-jsdom/esm/register"

module Colour =
    type State =
        | Red
        | Green
        | Blue

    type Model = Model<State, State>

    let create() =
        let init() = Red
        let update _ m = m
        Model(init, update)

type ITestAdapter =
    abstract RenderCount: int
    abstract Model: Colour.Model
    abstract CreateElement: unit -> ReactElement

let buildAdapter() =
    let mutable renders = 0
    let model = Colour.create()

    let Component = observer(fun () ->
        renders <- renders + 1
        div [Props.Data("testid", "colour")] [
            str (sprintf "%A" model.State)
        ]
    )

    { new ITestAdapter with
        member _.RenderCount = renders
        member _.Model = model
        member _.CreateElement() = React.createElement(Component, (), [])
    }

let getRenderedColour (result: ReactTestingLibrary.IRenderResult) =
    let element = result.getByTestId("colour")
    element.textContent

let renderTest f =
    fun () ->
        let adapter = buildAdapter()
        let result = ReactTestingLibrary.render(adapter.CreateElement())
        try f adapter result
        finally ReactTestingLibrary.cleanup()

let reactTests = testList "react" [
    testCase "should render initial state" <| renderTest (fun _ result ->
        Should |> Expect.equal (result |> getRenderedColour) "Red"
    )

    testCase "should render updated state" <| renderTest (fun adapter result ->
        adapter.Model.Post(Colour.Green)
        adapter.Model.Post(Colour.Blue)

        Should |> Expect.equal adapter.RenderCount 3
        Should |> Expect.equal (result |> getRenderedColour) "Blue"
    )

    testCase "should ignore identical state changes" <| renderTest (fun adapter _ ->
        adapter.Model.Post(adapter.Model.State)
        adapter.Model.Post(adapter.Model.State)

        Should |> Expect.equal adapter.RenderCount 1
    )

    testCase "should not rerender unchanged state" <| renderTest (fun adapter result ->
        result.rerender(adapter.CreateElement())
        result.rerender(adapter.CreateElement())

        Should |> Expect.equal adapter.RenderCount 1
    )
]

Mocha.runTests reactTests |> ignore
GlobalJsDom.cleanup()
