[<AutoOpen>]
module Utils

open Fable.Core
open Fable.React

[<ImportDefault("memoize-one")>]
let memoizeOnce<'t>(value: 't): 't = jsNative

[<ImportDefault("memoize-one")>]
let memoizeOnceWithEquality<'t>(value: 't, equaliyFn: obj[] -> obj[] -> bool): 't = jsNative

[<Emit("Array.isArray($0)")>]
let private isArray (x: obj) = jsNative

let inline private shallowArrayEquals (a: obj[]) (b: obj[]) =
    if a.Length <> b.Length then
        false
    else
        let mutable i = 0
        let mutable equals = true
        while i < a.Length && equals do
            let valueA = a.[i]
            let valueB = b.[i]
            equals <- System.Object.ReferenceEquals(valueA, valueB)
            i <- i + 1
        equals

let private tupledShallowArrayEquals (a: obj[]) (b: obj[]) =
    if a.Length = 1 && b.Length = 1 && isArray a.[0] && isArray b.[0] then
        shallowArrayEquals (unbox a.[0]) (unbox b.[0])
    else
        JS.console.error("tupledShallowArrayEquals called with non tupled arguments: ", a, b)
        failwithf "tupledShallowArrayEquals called with non tupled arguments: %A, %A" a b

let memoizeOnceTupled<'t>(value: 't): 't = memoizeOnceWithEquality(value, tupledShallowArrayEquals)

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