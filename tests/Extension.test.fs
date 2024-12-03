module Tests.Extension

open MobF
open Fable.Mocha

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
    test "should compute an extended value" {
        let model = Note.create "foo"
        let actual = model.Computed.Metrics.Characters
        Should |> Expect.equal actual 3
    }

    test "should persist the computed definitions" {
        let model = Note.create ""

        model.Computed.Metrics.Characters |> ignore
        model.Computed.Metrics.Characters |> ignore
        model.Computed.Metrics.Characters |> ignore

        Should |> Expect.equal 1 (model.Computed.Metrics.CreationCount)
    }

    test "should respond to state changes" {
        let model = Note.create ""
        let counter = countReactions (fun () -> model.Computed.Metrics.Characters)

        model.Post(Note.Edit "a")
        model.Post(Note.Edit "ab")
        model.Post(Note.Edit "abc")

        Should |> Expect.equal counter.Value 3
    }

    test "should support multiple extensions" {
        let model = Note.create "this is a test"

        let count = model.Computed.Metrics.Characters
        let html = model.Computed.Output.Html

        Should |> Expect.equal count 14
        Should |> Expect.equal html "<p>this is a test</p>"
    }
]

Mocha.runTests extensionTests |> ignore
