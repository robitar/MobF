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

        Model.useInit init
        |> Model.andUpdate update
        |> Model.create

type ITestAdapter =
    abstract RenderCount: int
    abstract Model: Colour.Model
    abstract CreateElement: unit -> ReactElement

module FunctionSetup =
    let create () =
        let mutable renders = 0
        let model = Colour.create()

        let ColourView = observer(fun () ->
            renders <- renders + 1
            div [Props.Data("testid", "colour")] [
                str (sprintf "%A" model.State)
            ]
        )

        { new ITestAdapter with
            member _.RenderCount = renders
            member _.Model = model
            member _.CreateElement() = React.createElement(ColourView, (), [])
        }

module ComponentSetup =
    type Counter() =
        member val Value = 0 with get, set
        member this.Inc() = this.Value <- this.Value + 1

    [<ObserverComponent>]
    let ColourView (props: {| RenderCount: Counter; Model: Colour.Model |}) =
        props.RenderCount.Inc()
        div [Props.Data("testid", "colour")] [
            str (sprintf "%A" props.Model.State)
        ]

    let create () =
        let props = {|
            RenderCount = Counter()
            Model = Colour.create()
        |}

        { new ITestAdapter with
            member _.RenderCount = props.RenderCount.Value
            member _.CreateElement() = ColourView props
            member _.Model = props.Model
        }

let getRenderedColour (result: ReactTestingLibrary.IRenderResult) =
    let element = result.getByTestId("colour")
    element.textContent

let renderTest (makeAdapter: unit -> ITestAdapter) f =
    fun () ->
        let adapter = makeAdapter()
        let result = ReactTestingLibrary.render(adapter.CreateElement())
        try f adapter result
        finally ReactTestingLibrary.cleanup()

let buildCases (makeAdapter: unit -> ITestAdapter) = 
    [
        testCase "should render initial state" <| renderTest makeAdapter (fun _ result ->
            Should |> Expect.equal (result |> getRenderedColour) "Red"
        )

        testCase "should render updated state" <| renderTest makeAdapter (fun adapter result ->
            adapter.Model.Post(Colour.Green)
            adapter.Model.Post(Colour.Blue)

            Should |> Expect.equal adapter.RenderCount 3
            Should |> Expect.equal (result |> getRenderedColour) "Blue"
        )

        testCase "should ignore identical state changes" <| renderTest makeAdapter (fun adapter _ ->
            adapter.Model.Post(adapter.Model.State)
            adapter.Model.Post(adapter.Model.State)

            Should |> Expect.equal adapter.RenderCount 1
        )

        testCase "should not rerender unchanged state" <| renderTest makeAdapter (fun adapter result ->
            result.rerender(adapter.CreateElement())
            result.rerender(adapter.CreateElement())

            Should |> Expect.equal adapter.RenderCount 1
        )
    ]

let reactTests = testList "react" [
    testList "function style" (buildCases FunctionSetup.create)
    testList "plugin style" (buildCases ComponentSetup.create)
]

Mocha.runTests reactTests |> ignore
GlobalJsDom.cleanup()
