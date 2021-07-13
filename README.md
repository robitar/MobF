# MobF

MobF is a functional reactive state management library for Fable based on
[MobX](https://github.com/mobxjs/mobx) and
[Elmish](https://github.com/elmish/elmish). It attempts to retain the functional
spirit of MVU, while embracing the capabilities of F# to reduce boilerplate and
ease component composition.

## Installation

MobF supports [Femto](https://github.com/Zaid-Ajaj/Femto), which will set up the
MobX dependency automatically:

```
femto install MobF
```

### Manual installation

.NET library:
```
dotnet add package MobF
```

npm package (note the 'x', this is mobx, the underlying JS library)
```
npm install mobx --save
```

## Design

Instead of maintaining a single immutable state tree and formalizing all message
passing explicitly, MobF allows you to compose individual models into an object
graph which can be restructured with minimal disruption.

Each model has an immutable `State` and accepts a `Msg` to perform updates. The
updates can be addressed directly to the model without the involvement of
containing/parent structures.

Consistency between models is maintained via `Computed` views, which react to changes
in the state, just as in regular MobX.

## Example

```F#
open MobF

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
            | Describe desc -> { state with Description = desc }
            | Complete -> { state with IsDone = true }
            | Restart -> { state with IsDone = false }

        Model(init, update)

module TaskList =
    type State = {
        Tasks: Task.Model list
    }

    type Msg =
        | Add of string
        | CompleteAll

    type Model = Model<State, Msg>

    let create () =
        let init () = {
            Tasks = []
        }

        let update state = function
            | Add desc ->
                let task = Task.create desc
                { state with Tasks = task :: state.Tasks }
                
            | CompleteAll ->
                state.Tasks |> List.iter (fun x -> x.Post(Task.Complete))
                state

        Model(init, update)

    let pendingCount (model: Model) =
        model.Computed(fun state -> 
            (0, state.Tasks) ||> List.fold (fun a x -> if x.State.IsDone then a else a + 1)
        )

    let summary (model: Model) =
        model.Computed(fun state ->
            state.Tasks |> List.map (fun x -> x.State.Description)
        )

let tasks = TaskList.create()

tasks.Post(TaskList.Add "foo")
tasks.Post(TaskList.Add "bar")

Reaction.autorun (fun () -> 
    printf "There are %i pending tasks" (tasks |> TaskList.pendingCount)
)

tasks.Post(TaskList.CompleteAll)
```

MobF also accepts a union type for the state, which is useful for modelling
async/input models.

```F#
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

let model = Input.create<string>()

model.Post(Input.Pending "Hello")
model.Post(Input.Accepted "Hello, World!")
```