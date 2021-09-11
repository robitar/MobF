module Tests.MobF

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

type ICounter =
    abstract Value: int

let countReactions f =
    let mutable value = -1
    Reaction.autorun (fun () -> f() |> ignore; value <- value + 1)
    { new ICounter with member _.Value = value }

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
            let model = Posting.create()

            let actual =
                match model.State.Messages with
                | ["init"; "post:a"; "post:b"] -> true
                | _ -> false

            Should |> Expect.isTrue actual

        testCase "should post from update" <| fun () ->
            let model = Posting.create()

            model.Post(Posting.Append "update")

            let actual =
                match model.State.Messages with
                | ["init"; "post:a"; "post:b"; "update"; "post:c"; "post:d"] -> true
                | _ -> false
                
            Should |> Expect.isTrue actual
    ]
]

Mocha.runTests modelTests |> ignore
