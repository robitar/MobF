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
        IsCompilerGenerated = true 
    }
    Fable.Import(declaration, Fable.Any, None)

let makeCall callee args =
    let callInfo: Fable.CallInfo = {
        ThisArg = None
        Args = args
        SignatureArgTypes = []
        HasSpread = false
        IsJsConstructor = false
        CallMemberInfo = None 
    }

    Fable.Call(callee, callInfo, Fable.Any, None)

type ValueMemberInfo(i: Fable.MemberInfo) =
    interface Fable.MemberInfo with
        member _.IsValue = true
        member _.Attributes = i.Attributes
        member _.HasSpread = i.HasSpread
        member _.IsPublic = i.IsPublic
        member _.IsInstance = i.IsInstance
        member _.IsMutable = i.IsMutable
        member _.IsGetter = false
        member _.IsSetter = false
        member _.IsEnumerator = i.IsEnumerator
        member _.IsMangled = i.IsMangled

let makeValueMember (name, info, body): Fable.MemberDecl = 
    {
        Name = name
        FullDisplayName = name
        Args = []
        Body = body
        UsedNames = Set.empty
        Info = ValueMemberInfo(info)
        ExportDefault = false
    }

let makeEmit macro args  =
    let info : Fable.EmitInfo = {
        Macro = macro
        IsJsStatement = false
        CallInfo = {
            ThisArg = None
            Args = args
            SignatureArgTypes = []
            HasSpread = false
            IsJsConstructor = false
            CallMemberInfo = None
        }
    }

    Fable.Expr.Emit(info, Fable.Type.Any, None)

let makeCreateElementCall elementType args =
    let createElement = makeImport "createElement" "react"
    
    let callInfo: Fable.CallInfo = {
        ThisArg = None
        Args = args
        SignatureArgTypes = []
        HasSpread = false
        IsJsConstructor = false
        CallMemberInfo = None
    }

    Fable.Call(createElement, callInfo, elementType, None)

let recordHasField name (compiler: PluginHelper) = function
    | Fable.Type.AnonymousRecordType (fieldNames, _) ->
        fieldNames
        |> Array.exists (fun field -> field = name)

    | Fable.Type.DeclaredType (entity, _) ->
        compiler.GetEntity(entity).FSharpFields
        |> List.exists (fun field -> field.Name = name)

    | _ ->
        false

let nullValue = Fable.Expr.Value(Fable.ValueKind.Null(Fable.Type.Any), None)
