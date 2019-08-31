module Utils

open Fable.React

let inline functionComponent name render = FunctionComponent.Of(render, name)
let inline elmishView name render = FunctionComponent.Of(render, name, equalsButFunctions)
