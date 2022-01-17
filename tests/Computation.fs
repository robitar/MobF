module Tests.Computation

open MobF
open Fable.Mocha

module Analysis =
    type IPrimativeMetrics =
        abstract Characters: int

    type MetricInfo = {
        Characters: int
    }

    type IComplexMetrics =
        abstract Values: MetricInfo

    let computeComplex (state: Note.State) =
        { new IComplexMetrics with
            member _.Values = { Characters = state.Text.Length }
        }

    type Note.IComputed with
        member computed.PrimativeMetrics = computed.Extend(fun state ->
            { new IPrimativeMetrics with
                member _.Characters = state.Text.Length
            }
        )

        member computed.DefaultComplex = computed.Extend computeComplex
        member computed.StructuralComplex = computed.Extend (computeComplex >> Computation.structural)

open Analysis

let computedTests = testList "computation" [
    let postEditsTo (note: Note.Type) =
        note.Post(Note.Edit "def")
        note.Post(Note.Edit "ghi")
        note.Post(Note.Edit "jkl")

    testList "reactivity when computed results are logically equal" [
        testCase "should not react for primative values" <| fun () ->
            let note = Note.create "abc"
            
            let reactions = countReactions (fun () -> 
                note.Computed.PrimativeMetrics.Characters
            )

            postEditsTo note

            Should |> Expect.equal reactions.Value 0

        testCase "should react for complex values by default" <| fun () ->
            let note = Note.create "abc"

            let reactions = countReactions (fun () -> 
                note.Computed.DefaultComplex.Values.Characters
            )

            postEditsTo note

            Should |> Expect.equal reactions.Value 3

        testCase "should not react for structural values" <| fun () ->
            let note = Note.create "abc"

            let reactions = countReactions (fun () ->
                note.Computed.StructuralComplex.Values.Characters
            )

            postEditsTo note

            Should |> Expect.equal reactions.Value 0
    ]
]

Mocha.runTests computedTests |> ignore
