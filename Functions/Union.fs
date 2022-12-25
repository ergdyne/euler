module Functions.Union

open Microsoft.FSharp.Reflection

let listAll<'T>() =
    FSharpType.GetUnionCases(typeof<'T>)
    |> Seq.map (fun x -> FSharpValue.MakeUnion(x, Array.zeroCreate(x.GetFields().Length)) :?> 'T)
    |> List.ofSeq
