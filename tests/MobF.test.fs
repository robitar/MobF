﻿module Tests.MobF

open System

open MobF
open Fable.Mocha

module Task =
    type State = {
        Description: string
        IsDone: bool
    }

    type Msg =
        | Describe of string
        | Complete
        | Restart

    type Model = Model<State, Msg>

    let create description: Model =
        let init () = {
            Description = description
            IsDone = false
        }

        let update state = function
            | Describe name -> { state with Description = name }
            | Complete -> { state with IsDone = true }
            | Restart -> { state with IsDone = false }

        Model.useInit init
        |> Model.andUpdate update
        |> Model.create

module TaskList =
    type State = {
        Tasks: Task.Model list
    }

    type Msg =
        | Add of string

    type IComputed =
        abstract PendingCount: int

    type Model = Model<State, Msg, IComputed>

    let create () : Model =
        let init () = {
            Tasks = []
        }

        let update state = function
            | Add name ->
                let task = Task.create name
                { state with Tasks = task :: state.Tasks }

        let compute state =
            { new IComputed with
                member _.PendingCount =
                    (0, state.Tasks)
                    ||> List.fold (fun a x -> if x.State.IsDone then a else a + 1)
            }

        Model.useInit init
        |> Model.andUpdate update
        |> Model.createWithComputed compute

module ClassTask =
    type State() = class end
    type Msg = Msg
    let init () = State()
    let update state _ = state

module Input =
    type State<'T> =
        | Idle
        | Pending of 'T
        | Accepted of 'T
        | Rejected

    type Model<'T> = Model<State<'T>, State<'T>>

    let inline create<'T> () =
        let init () = Idle
        let update _ m = m

        Model.useInit init
        |> Model.andUpdate update
        |> Model.create

module Posting =
    type State = {
        Messages: string list
        PostFromInit: string -> unit
    }

    type Msg =
        | Append of string
        | PostAppend of string

    type Model = Model<State, Msg>

    let create () : Model =
        let init post =
            post (PostAppend "a")
            post (PostAppend "b")
            {
                Messages = ["init"]
                PostFromInit = PostAppend >> post
            }

        let update (state, post) = function
            | Append message ->
                post (PostAppend "c")
                post (PostAppend "d")
                { state with Messages = state.Messages @ [message] }

            | PostAppend message ->
                let formatted = sprintf "post:%s" message
                { state with Messages = state.Messages @ [formatted] }

        Model.useInitWithPost init
        |> Model.andUpdateWithPost update
        |> Model.create

module Autosub =
    type State = {
        One: int
        Two: int
    }

    type Msg =
        | SubscribeOne of Type
        | SubscribeTwo of Type
        | SetOne of int
        | SetTwo of int
        | Invoke of (unit -> unit)

    and Type = Model<State, Msg>

    let create () =
        let init () =
            {
                One = 0
                Two = 0
            }

        let update (state, post) = function
            | SubscribeOne target ->
                target |> Subscribe.auto (fun x -> x.One) (SetOne >> post)
                state

            | SubscribeTwo target ->
                target |> Subscribe.auto (fun x -> x.Two) (SetTwo >> post)
                state

            | SetOne x ->
                { state with One = x }

            | SetTwo x ->
                { state with Two = x }

            | Invoke action ->
                action()
                state

        Model.useInit init
        |> Model.andUpdateWithPost update
        |> Model.create

let modelTests = testList "model" [
    testList "record types" [
        testCase "should create a model" <| fun () ->
            let model = Task.create "foo"

            Should |> Expect.isFalse model.State.IsDone
            Should |> Expect.equal model.State.Description "foo"

        testCase "should update a model" <| fun () ->
            let model = Task.create "x"

            model.Post(Task.Describe "renamed")

            Should |> Expect.equal (model.State.Description) "renamed"

        testList "reactivity" [
            testCase "should respond to changes" <| fun () ->
                let model = Task.create "x"
                let counter = countReactions (fun () -> model.State.Description)

                model.Post(Task.Describe "a")
                model.Post(Task.Describe "b")
                model.Post(Task.Describe "c")

                Should |> Expect.equal 3 counter.Value

            testCase "should ignore unchanged fields" <| fun () ->
                let model = Task.create "x"
                let counter = countReactions (fun () -> model.State.IsDone)

                model.Post(Task.Restart)
                model.Post(Task.Restart)
                model.Post(Task.Restart)

                Should |> Expect.isZero counter.Value

            testCase "should ignore unrelated fields" <| fun () ->
                let model = Task.create "x"
                let countChangesToDone = countReactions (fun () -> model.State.IsDone)
                let countChangesToName = countReactions (fun () -> model.State.Description)

                model.Post(Task.Complete)
                model.Post(Task.Restart)
                model.Post(Task.Complete)

                Should |> Expect.equal 3 countChangesToDone.Value
                Should |> Expect.isZero countChangesToName.Value
        ]
    ]

    testList "union types" [
        testCase "should create a model" <| fun () ->
            let model = Input.create()

            Should |> Expect.equal Input.Idle model.State

        testCase "should update a model" <| fun () ->
            let model = Input.create<int>()
            model.Post(Input.Accepted 5)

            Should |> Expect.equal (Input.Accepted 5) model.State

        testList "reactivity" [
            testCase "should respond to changes" <| fun () ->
                let model = Input.create()
                let counter = countReactions (fun () -> model.State)

                model.Post(Input.Pending 5)
                model.Post(Input.Accepted 5)

                Should |> Expect.equal 2 counter.Value

            testCase "should disregard successive identical states" <| fun () ->
                let model = Input.create()
                let counter = countReactions (fun () -> model.State)

                model.Post(Input.Idle)
                model.Post(Input.Idle)
                model.Post(Input.Idle)

                Should |> Expect.isZero counter.Value
        ]
    ]

    testList "class types" [
        testCase "should reject a class type" <| fun () ->
            expectThrows (fun () ->
                Model.useInit ClassTask.init
                |> Model.andUpdate ClassTask.update
                |> Model.create
                |> ignore
            )
    ]

    testList "computed" [
        let buildModel () =
            let model = TaskList.create()

            model.Post(TaskList.Add "a")
            model.Post(TaskList.Add "b")
            model.Post(TaskList.Add "c")

            model

        testCase "should evaluate a computation" <| fun () ->
            let model = buildModel()
            let pending = model.Computed.PendingCount

            Should |> Expect.equal 3 pending

        testCase "should react to computations" <| fun () ->
            let model = buildModel()
            let counter = countReactions (fun () -> model.Computed.PendingCount)

            model.State.Tasks |> List.iter (fun t -> t.Post(Task.Complete))

            Should |> Expect.equal 3 counter.Value
    ]

    testList "post" [
        testCase "should post from init" <| fun () ->
            let model = Posting.create ()

            let actual =
                match model.State.Messages with
                | ["init"; "post:a"; "post:b"] -> true
                | _ -> false

            Should |> Expect.isTrue actual

        testCase "should dispatch after init completes" <| fun () ->
            let model = Posting.create ()
            model.State.PostFromInit "after-init"

            let actual =
                match model.State.Messages with
                | ["init"; "post:a"; "post:b"; "post:after-init"] -> true
                | _ -> false

            Should |> Expect.isTrue actual

        testCase "should post from update" <| fun () ->
            let model = Posting.create ()

            model.Post(Posting.Append "update")

            let actual =
                match model.State.Messages with
                | ["init"; "post:a"; "post:b"; "update"; "post:c"; "post:d"] -> true
                | _ -> false

            Should |> Expect.isTrue actual
    ]

    testList "subscription" [
        testList "manual" [
            let effectCounter selector (model: Task.Model) =
                let mutable count = 0
                let sub = model |> Subscribe.manual selector (fun _ -> count <- count + 1)
                { new IDisposableCounter with
                    member _.Value = count
                    member _.Dispose () = sub.Dispose()
                }

            let selectDescription (state: Task.State) =
                state.Description

            let postAbc (model: Task.Model) =
                model.Post(Task.Describe "a")
                model.Post(Task.Describe "b")
                model.Post(Task.Describe "c")

            testCase "should trigger effects" <| fun () ->
                let model = Task.create "foo"

                let counter = model |> effectCounter selectDescription
                model |> postAbc

                Should |> Expect.equal counter.Value 3

            testCase "should trigger effects immediately" <| fun () ->
                let model = Task.create "foo"
                let mutable triggered = false

                model
                |> Subscribe.manualImmediate
                    selectDescription
                    (fun x -> if x = "foo" then triggered <- true)
                |> ignore

                Should |> Expect.isTrue triggered

            testCase "should not trigger effects if data does not change" <| fun () ->
                let model = Task.create "foo"
                let counter = model |> effectCounter selectDescription

                model.Post(Task.Describe "foo")
                model.Post(Task.Describe "foo")
                model.Post(Task.Describe "foo")

                Should |> Expect.equal 0 counter.Value

            testCase "should provide updated value to the effect" <| fun () ->
                let model = Task.create "foo"

                let mutable values = List.empty
                model |> Subscribe.manual selectDescription (fun x -> values <- x :: values) |> ignore
                model |> postAbc

                Should |> Expect.equal ["c"; "b"; "a"] values

            testCase "should cancel subscriptions" <| fun () ->
                let model = Task.create "foo"
                let counter = model |> effectCounter selectDescription

                counter.Dispose()
                model |> postAbc

                Should |> Expect.equal 0 counter.Value

            testCase "should cancel all subscriptions when model is disposed" <| fun () ->
                let model = Task.create "foo"

                let c1 = model |> effectCounter selectDescription
                let c2 = model |> effectCounter selectDescription
                let c3 = model |> effectCounter selectDescription

                (model :> IDisposable).Dispose()

                model |> postAbc

                Should |> Expect.equal 0 c1.Value
                Should |> Expect.equal 0 c2.Value
                Should |> Expect.equal 0 c3.Value

            testCase "should tolerate multiple attempts to dispose" <| fun () ->
                let model = Task.create "foo"

                let c1 = model |> effectCounter selectDescription
                let c2 = model |> effectCounter selectDescription

                c1.Dispose()
                (model :> IDisposable).Dispose()
                c2.Dispose()

                model |> postAbc

                Should |> Expect.equal 0 c1.Value
                Should |> Expect.equal 0 c2.Value
        ]

        testList "auto" [
            let makePair () = (Autosub.create(), Autosub.create())
            let makeThree () = (Autosub.create(), Autosub.create(), Autosub.create())

            let subOne (target: Autosub.Type) (listener: Autosub.Type) = listener.Post(Autosub.SubscribeOne target)
            let subTwo (target: Autosub.Type) (listener: Autosub.Type) = listener.Post(Autosub.SubscribeTwo target)

            let setOne value (m: Autosub.Type) = m.Post(Autosub.SetOne value)
            let setTwo value (m: Autosub.Type) = m.Post(Autosub.SetTwo value)

            let assertActive (expected: bool) (listener: Subscribe.ISubscribable<_>) = Should |> Expect.equal expected listener.IsActive
            let shouldBeActive listener = assertActive true listener
            let shouldNotBeActive listener = assertActive false listener

            testCase "should activate listener and target" <| fun () ->
                let (listener, target) = makePair ()
                listener |> subOne target

                listener |> shouldBeActive
                target |> shouldBeActive

            testCase "should cancel the subscription when the listener is disposed" <| fun () ->
                let (listener, target) = makePair ()

                listener |> subOne target
                listener |> Model.dispose

                listener |> shouldNotBeActive
                target |> shouldNotBeActive

            testCase "should cancel the subscription when the target is disposed" <| fun () ->
                let (listener, target) = makePair ()

                listener |> subOne target
                target |> Model.dispose

                listener |> shouldNotBeActive
                target |> shouldNotBeActive

            testCase "should cancel all subscriptions when the target is disposed" <| fun () ->
                let (a, b, c) = makeThree ()

                a |> subOne c
                b |> subOne c

                c |> Model.dispose

                a |> shouldNotBeActive
                b |> shouldNotBeActive

            testCase "should trigger effects" <| fun () ->
                let (a, b) = makePair ()

                a |> subOne b
                b |> setOne 123

                Should |> Expect.equal 123 a.State.One

            testCase "should capture multiple subscriptions" <| fun () ->
                let (a, b, c) = makeThree ()

                a |> subOne b
                a |> subTwo c

                b |> setOne 123
                c |> setTwo 456

                Should |> Expect.equal 123 a.State.One
                Should |> Expect.equal 456 a.State.Two

            testCase "should attach subscriptions to the right model in nested actions" <| fun () ->
                let (listener, target) = makePair ()

                listener.Post(Autosub.Invoke (fun () ->
                    target |> subOne listener

                    target.Post(Autosub.Invoke (fun () ->
                        listener |> subTwo target
                    ))
                ))

                listener |> setOne 123
                target |> setTwo 456

                Should |> Expect.equal 123 target.State.One
                Should |> Expect.equal 456 listener.State.Two
        ]
    ]
]

Mocha.runTests modelTests |> ignore
