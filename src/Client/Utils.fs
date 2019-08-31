[<AutoOpen>]
module Utils

open Fable.React

type ComponentEquality =
    | ByRef
    | ByValue

let inline elmishView name equality render =
    match equality with
    | ByValue -> FunctionComponent.Of(render, name, equalsButFunctions)
    | ByRef -> FunctionComponent.Of(render, name, memoEqualsButFunctions)