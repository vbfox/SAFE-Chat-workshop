[<AutoOpen>]
module Utils

open Fable.Core
open Fable.React

[<StringEnum>]
type ComponentEquality =
    | ByRef
    | ByValue
    | NoMemoization

let inline elmishView name equality render =
    match equality with
    | ByValue -> FunctionComponent.Of(render, name, equalsButFunctions)
    | ByRef -> FunctionComponent.Of(render, name, memoEqualsButFunctions)
    | NoMemoization -> FunctionComponent.Of(render, name)