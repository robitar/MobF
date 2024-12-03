module Tests.Reactive

open MobF
open Fable.Mocha

let reactiveTests = testList "reactive" [
    let setup () =
        let mutable count = 0

        let a = Note.create ""
        let b = Note.create ""
        let c = Note.create ""

        let sequence = Reactive.seq(fun () ->
            count <- count + 1
            [
                a.Computed.IsEmpty
                b.Computed.IsEmpty
                c.Computed.IsEmpty
            ]
        )

        let counter = { new ICounter with member _.Value = count }
        a, b, c, sequence, counter

    testList "seq" [
        test "should generate a sequence" {
            let _, _, _, sequence, _ = setup ()

            Should |> Expect.equal (sequence |> Seq.length) 3
        }

        test "should invoke the generator once when in a reactive context" {
            let _, _, _, sequence, counter = setup ()

            Reactive.autorun (fun () ->
                Seq.length sequence |> ignore
                Seq.length sequence |> ignore
                Seq.length sequence |> ignore
            )
            |> ignore

            Should |> Expect.equal counter.Value 1
        }

        test "should react to changes" {
            let a, b, c, sequence, _ = setup ()

            let reactions = countReactions (fun () ->
                sequence |> Seq.length |> ignore
            )

            a.Post(Note.Edit "1")
            b.Post(Note.Edit "2")
            c.Post(Note.Edit "3")

            Should |> Expect.equal reactions.Value 3
        }

        test "should regenerate values when state changes" {
            let a, _, c, sequence, _ = setup ()

            Should |> Expect.isTrue (sequence |> List.ofSeq |> (=) [true; true; true])
            
            a.Post(Note.Edit "1")
            c.Post(Note.Edit "3")

            Should |> Expect.isTrue (sequence |> List.ofSeq |> (=) [false; true; false])
        }
    ]
]

Mocha.runTests reactiveTests |> ignore
