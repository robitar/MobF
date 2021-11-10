# MobF

[![nuget: MobF](https://img.shields.io/nuget/v/MobF?label=nuget%3A%20MobF)](https://www.nuget.org/packages/MobF)

[![nuget: MobF.React](https://img.shields.io/nuget/v/MobF.React?label=nuget%3A%20MobF.React)](https://www.nuget.org/packages/MobF.React)

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

Consistency between models is maintained via `Computed` views and subscriptions,
which react to changes in the state, just as in regular MobX.

## Example

```F#
open System
open MobF

module Task =
    type State = {
        Id: Guid
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
            Id = Guid.NewGuid()
            Description = description
            IsDone = false
        }

        let update state = function
            | Describe desc -> { state with Description = desc }
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
        | CompleteAll

    type IComputed =
        abstract PendingCount: int

    type Model = Model<State, Msg, IComputed>

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

        let compute state =
            { new IComputed with
                member _.PendingCount =
                    (0, state.Tasks)
                    ||> List.fold (fun a x -> if x.State.IsDone then a else a + 1)
            }

        Model.useInit init
        |> Model.andUpdate update
        |> Model.createWithComputed compute

let tasks = TaskList.create()

tasks.Post(TaskList.Add "foo")
tasks.Post(TaskList.Add "bar")

Reaction.autorun (fun () ->
    printf "There are %i pending tasks" tasks.Computed.PendingCount
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

        Model.useInit init
        |> Model.andUpdate update
        |> Model.create

let model = Input.create<string>()

model.Post(Input.Pending "Hello")
model.Post(Input.Accepted "Hello, World!")
```

## Complex init function

`init` can accept a function of type `Msg -> unit` which allows you to post a
message to the model instance being created. The messages are queued and execute
after `init` has completed.

```F#
let init post =
    post (TaskList.Add "Do a thing")
    post (TaskList.Add "Do another thing")

    {
        Tasks = []
    }
```

This is particularly useful for setting up subscriptions, such as timers or
event handlers.

```F#
let init post =
    setInterval (fun () -> post (TaskList.Add "Tick...")) 1000
    //...
```

When using init with post, you build up the model slightly differently

```F#
Model.useInitWithPost init
|> Model...
```

## Complex update function

Like `init`, `update` can accept a post function, but in addition, you can also
accept the computed view. These additional parameters are passed as a tuple
along with the state.

```F#
let update state msg =
    //simple form

let update (state, post) msg =
    //state with post

let update (state, post, computed: IComputed) msg =
    //state, post and computed
```

As with `init`, these forms require a slightly different model build-up.

```F#
Model.useInit init
|> Model.andUpdateWithPost update

Model.useInit init
|> Model.andUpdateWithComputed update
```

## Subscriptions

Subscriptions allow you to trigger effects when observable data changes. The
`Subscribe` module provides two kinds, manual and automatic. A manual
subscription is active until explicitly disposed. An automatic subscription is
bound to the lifetimes of the listener and target, when either of them are
disposed, the subscription is cancelled.

Automatic subscriptions can only be registered within the context of an `init`
or `update` function (or any functions called by them); where the current model
serves as the listener.

```F#
let task = Task.create "Do a thing"

//listen for changes to the 'Description' field on the task
let sub =
    task |> Subscribe.manual
        (fun s -> s.Description)
        (fun value -> printfn "Description is %s" value)

task.Post (Task.Describe "foo")
task.Post (Task.Describe "bar")

//cancel the subscription
sub.Dispose()
```

```F#
type State = {
    Description: string
}

type Msg =
    | Init
    | UpdateDescription of string

//...

let update (state, post) = function
    | Init ->
        //register an auto subscription which will remain active until
        //our model (listener), or the task (target), are disposed
        task |> Subscribe.auto
            (fun s -> s.Description)
            (fun desc -> post (UpdateDescription desc))
        state

    | UpdateDescription desc ->
        { state with Description = desc }
```

To trigger the effect immediately upon registering, use the 'immediate' variants
`manualImmediate` and `autoImmediate`. Otherwise, the subscriptions will only
respond to changes which occur after the registration has been made.

Note that effects are only triggered when the observed value changes, not
necessarily when attempts are made to update it. In the example of manual
subscriptions above, setting the description to `"Do a thing"` five times would
not trigger the subscription, because the effective value is identical.

The `Model` type implements `IDisposable`, and calling `Dispose` will cancel all
active and manual subscriptions it is currently participating in.

## Debugging

Each model defines a javascript property called `debugView` which will render
the state with `sprintf "%A"`. Nested models are handled gracefully.

```
> console.log(tasks.debugView)
--
{ Tasks = [{ Id = 33c2086f-12d4-41d3-8d33-e169467fcf86
  Description = Also do other things
  IsDone = true }; { Id = 0fde59af-a68f-4492-ae72-0c78bb7c94bf
  Description = Frob the things
  IsDone = true }] }
```

When compiled with Debug configuration, MobF messages are invoked in named
actions which can be observed in mobx-devtools. In Release configuration, the
actions are not named, to avoid serialization costs.

# React Integration

One of the more compelling features of MobX is the integration for React, which
provides fine-grained memoization and reactive updates with very little effort.

MobF.React is a thin wrapper over
[mobx-react-lite](https://github.com/mobxjs/mobx/tree/main/packages/mobx-react-lite).
It provides two mechanisms for creating react components, a function called
`observer`, and a Fable plugin attribute similar to (and directly inspired by)
`[<ReactComponent>]` in Feliz.

## Installation

MobF.React supports [Femto](https://github.com/Zaid-Ajaj/Femto), which will set
up the `mobx-react-lite` dependency automatically:

```
femto install MobF.React
```

### Manual installation

.NET library:
```
dotnet add package MobF.React
```

npm package
```
npm install mobx-react-lite --save
```

> Note: this assumes `react` and `react-dom` are already installed, both are
> required

## Usage

### Fable Plugin

Decorating a function with `[<ObserverComponent>]` allows you to define
components in a more declarative style. It also gives some additional benefits:

- A `displayName` is given to the component, which greatly improves the React
  devtools experience.
- Invocations of the function are tranformed into `React.createElement` calls,
  which allows for a more natural usage.
- If the props argument has a PascalCased field `Key`, a camelCase `key` field
  is synthesized, which becomes the React element key.

```F#
open Fable.React
open Fable.React.Props

open MobF.React

//the function must take a single argument and return a value of ReactElement
[<ObserverComponent>]
let TaskView (props: {| Task: Task.Model; Key: string |}) =
    let m = props.Task

    //unlike 'pure' elmish, there is no dispatch function, instead you post
    //messages directly to the target model
    li [OnClick (fun _ -> m.Post(Task.Complete))] [
        let style =
            match m.State.IsDone with
            | true -> Style [Color "#ccc"]
            | false -> Style []

        span [style] [str m.State.Description]
    ]

[<ObserverComponent>]
let TaskListView (props: {| TaskList: TaskList.Model |}) =
    let m = props.TaskList

    div [] [
        h2 [] [
            str $"There are %i{m |> TaskList.pendingCount} pending tasks"
        ]
        ul [] [
            for t in m.State.Tasks do
                //invocations of the function are automatically transformed into
                //calls to React.createElement, also note that a key is given
                TaskView {| Task = t; Key = t.State.Id.ToString() |}
        ]
    ]

let tasks = TaskList.create()
tasks.Post(TaskList.Add "Frob the things")
tasks.Post(TaskList.Add "Also do other things")

TaskListView {| TaskList = tasks |} |> mountById "root"

```

### Decorator Function

The function `observer`  wraps the view definition (in React parlance, a 'higher
order component'). You then use the result of this application as you would any
other component.

```F#
open MobF.React

let Component = observer(fun () ->
    div [] [
        //...
    ]
)
```

Using the task list model above, you can define the following reactive view:

```F#
let tasks = TaskList.create()

tasks.Post(TaskList.Add "Frob the things")
tasks.Post(TaskList.Add "Also do other things")

open Browser
open Fable.React
open Fable.React.Props
open Fable.React.ReactBindings

open MobF.React

type Props = {
    Todo: TaskList.Model
}

//note the observer function wrapping the view definition
let View = observer(fun (props: Props) ->
    div [] [
        h2 [] [
            str $"There are %i{props.Todo |> TaskList.pendingCount} pending tasks"
        ]
        ul [] [
            for task in props.Todo.State.Tasks do
                //unlike 'pure' elmish, there is no dispatch function, instead
                //you post messages directly to the target model
                li [OnClick (fun _ -> task.Post(Task.Complete))] [
                    let style =
                        match task.State.IsDone with
                        | true -> Style [Color "#ccc"]
                        | false -> Style []

                    span [style] [str task.State.Description]
                ]
        ]
    ]
)

ReactDom.render(
    React.createElement(View, { Todo = tasks }, []),
    document.getElementById("root")
)

```
