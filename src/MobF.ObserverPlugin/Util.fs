[<AutoOpen>]
module Util

open Fable
open Fable.AST

let makeIdent name: Fable.Ident = {
    Name = name
    Type = Fable.Any
    IsCompilerGenerated = true
    IsThisArgument = false
    IsMutable = false
    Range = None 
}

let makeStringConstant (x: string) =
    let s = Fable.StringConstant x
    Fable.Value(s, None)

let isReactElement = function
    | Fable.Type.DeclaredType (entity, _) -> entity.FullName.EndsWith "ReactElement"
    | _ -> false

let makeImport selector path =
    let declaration: Fable.ImportInfo = {
        Selector = selector
        Path = path
        Kind = Fable.UserImport(false)
    }
    Fable.Import(declaration, Fable.Any, None)

let makeCallInfo args: Fable.CallInfo =
    {
        ThisArg = None
        Args = args
        SignatureArgTypes = []
        GenericArgs = []
        MemberRef = None
        Tags = []
    }

let makeCall callee args typ =
    let info = makeCallInfo args
    Fable.Call(callee, info, typ, None)

let makeObjValueMemberInfo name typ: Fable.GeneratedMemberInfo =
    {
        Name = name
        ParamTypes = []
        ReturnType = typ
        IsInstance = true
        HasSpread = false
        IsMutable = false
        DeclaringEntity = None
    }

let makeValueMember (name, body): Fable.ObjectExprMember = 
    {
        Name = name
        Args = []
        Body = body
        MemberRef =
            makeObjValueMemberInfo name body.Type
            |> Fable.GeneratedValue
            |> Fable.GeneratedMemberRef
        IsMangled = false
    }

let makeEmit macro args  =
    let info : Fable.EmitInfo = {
        Macro = macro
        IsStatement = false
        CallInfo = makeCallInfo args
    }

    Fable.Expr.Emit(info, Fable.Type.Any, None)

let makeCreateElementCall elementType args =
    let createElement = makeImport "createElement" "react"
    makeCall createElement args elementType

let recordHasField name (compiler: PluginHelper) = function
    | Fable.Type.AnonymousRecordType (fieldNames, _, _) ->
        fieldNames
        |> Array.exists (fun field -> field = name)

    | Fable.Type.DeclaredType (entity, _) ->
        compiler.GetEntity(entity).FSharpFields
        |> List.exists (fun field -> field.Name = name)

    | _ ->
        false

let nullValue = Fable.Expr.Value(Fable.ValueKind.Null(Fable.Type.Any), None)
