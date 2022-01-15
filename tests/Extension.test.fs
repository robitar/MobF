module Tests.Extension

open MobF
open Fable.Mocha

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

module Analysis =
    type IMetrics =
        abstract Characters: int
        abstract CreationCount: int

    type Note.IComputed with
        member computed.Metrics =
            let mutable count = 0
            computed.Extend(fun state ->
                count <- count + 1
                { new IMetrics with
                    member _.Characters =
                        if computed.IsEmpty then 0
                        else state.Text.Length

                    member _.CreationCount =
                        count
                }
            )

module Output =
    type IOutput =
        abstract Html: string

    type Note.IComputed with
        member computed.Output = computed.Extend(fun state ->
            { new IOutput with
                member _.Html = sprintf "<p>%s</p>" state.Text
            }
        )

open Analysis
open Output

let extensionTests = testList "extension" [
    testCase "should compute an extended value" <| fun () ->
        let model = Note.create "foo"
        let actual = model.Computed.Metrics.Characters
        Should |> Expect.equal actual 3

    testCase "should persist the computed definitions" <| fun () ->
        let model = Note.create ""

        model.Computed.Metrics.Characters |> ignore
        model.Computed.Metrics.Characters |> ignore
        model.Computed.Metrics.Characters |> ignore

        Should |> Expect.equal 1 (model.Computed.Metrics.CreationCount)

    testCase "should respond to state changes" <| fun () ->
        let model = Note.create ""
        let counter = countReactions (fun () -> model.Computed.Metrics.Characters)

        model.Post(Note.Edit "a")
        model.Post(Note.Edit "ab")
        model.Post(Note.Edit "abc")

        Should |> Expect.equal counter.Value 3

    testCase "should support multiple extensions" <| fun () ->
        let model = Note.create "this is a test"

        let count = model.Computed.Metrics.Characters
        let html = model.Computed.Output.Html

        Should |> Expect.equal count 14
        Should |> Expect.equal html "<p>this is a test</p>"
]

Mocha.runTests extensionTests |> ignore
