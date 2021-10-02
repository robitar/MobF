namespace MobF.React

open Fable
open Fable.AST

[<assembly:ScanForPlugins>]
do ()

// Inspired by the ReactComponent plugin from Feliz!

/// <summary>
///     When applied to a function, transforms that function into a memoized,
///     reactive component (via mobx-react-lite).
/// </summary>
/// <remarks>
///    In addition to transforming the function into an observer, this plugin
///    will also set the displayName, which greatly improves the experience in
///    React dev tools. If the function argument is a record with a field named
///    'Key', that field will be used as the React element key.
/// </remarks>
type ObserverComponentAttribute() =
    inherit MemberDeclarationPluginAttribute()

    override _.FableMinimumVersion = "3.0"

    override _.TransformCall(compiler, _, expr) =
        let elementType = expr.Type

        match expr with
        | Fable.Call(componentType, info, _, _) ->
            let arg = info.Args.[0]

            let props =
                if arg.Type = Fable.Type.Unit then
                    nullValue
                elif recordHasField "Key" compiler arg.Type then
                    makeEmit "(($x) => ({ ...$x, key: $x.Key }))($0)" [arg]
                else
                    arg

            makeCreateElementCall elementType [componentType; props]

        | _ -> expr


    override _.Transform(compiler, _, decl) =
        let error reason =
            let message = sprintf "'%s' is not a valid [<ObserverComponent>] because %s" decl.Name reason
            compiler.LogWarning(message, ?range=decl.Body.Range)

        if decl.Info.IsValue || decl.Info.IsGetter || decl.Info.IsSetter then
            error "it is not a function"
            decl

        else if not (isReactElement decl.Body.Type) then
            error (sprintf "it should produce ReactElement, but instead produces <%A>" decl.Body.Type)
            decl

        else if decl.Args.Length > 1 then
            error "it has more than one argument, React expects a single 'props' argument"
            decl

        else
            let ident = makeIdent "x"

            let observer = makeImport "observer" "mobx-react-lite"
            let lambda = Fable.Lambda(decl.Args |> List.head, decl.Body, Some decl.Name)
            let call = makeCall observer [lambda]

            let identExpr = Fable.IdentExpr ident
            let setDisplayName = makeEmit "$0.displayName = $1" [identExpr; makeStringConstant decl.Name]

            let body = Fable.Let(ident, call, Fable.Sequential [setDisplayName; identExpr])

            makeValueMember(decl.Name, decl.Info, body)
