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

    let create description =
        let init () = {
            Description = description
            IsDone = false
        }

        let update state = function
            | Describe name -> { state with Description = name }
            | Complete -> { state with IsDone = true }
            | Restart -> { state with IsDone = false }

        Model(init, update)

module TaskList =
    type State = {
        Tasks: Task.Model list
    }

    type Msg =
        | Add of string

    type Model = Model<State, Msg>

    let create () =
        let init () = {
            Tasks = []
        }

        let update state = function
            | Add name ->
                let task = Task.create name
                { state with Tasks = task :: state.Tasks }

        Model(init, update)

    let pendingCount (model: Model) =
        model.Computed(fun state -> 
            (0, state.Tasks) ||> List.fold (fun a x -> if x.State.IsDone then a else a + 1)
        )

    let items (model: Model) =
        model.Computed(fun state ->
            state.Tasks |> List.map (fun x -> x.State.Description)
        )

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
        Model<'T>(init, update)

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
            expectThrows (fun () -> Model(ClassTask.init, ClassTask.update) |> ignore)
    ]

    testList "computed" [
        let buildModel () =
            let model = TaskList.create()

            model.Post(TaskList.Add "a")
            model.Post(TaskList.Add "b")
            model.Post(TaskList.Add "c")

            model

        testCase "can evaluate a computation" <| fun () ->
            let model = buildModel()
            let pending = model |> TaskList.pendingCount 

            Should |> Expect.equal 3 pending

        testCase "should react to computations" <| fun () ->
            let model = buildModel()
            let counter = countReactions (fun () -> model |> TaskList.pendingCount)

            model.State.Tasks |> List.iter (fun t -> t.Post(Task.Complete))

            Should |> Expect.equal 3 counter.Value
    ]
]

Mocha.runTests modelTests |> ignore
