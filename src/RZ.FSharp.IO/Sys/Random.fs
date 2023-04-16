namespace RZ.FSharp.IO.Sys

open System.Runtime.CompilerServices

type Random =
    abstract member next: int -> int
    abstract member nextDouble: unit -> float
    
type HasRandom =
    abstract member random: Random
    
[<IsReadOnly; Struct; NoEquality; NoComparison>]
type RealRandom(rand: System.Random) =
    interface Random with
        member _.next N = rand.Next N
        member _.nextDouble() = rand.NextDouble()
        
module Random =
    open RZ.FSharp.IO
    let inline next N       = fun (env: #HasRandom) -> ioOk(env.random.next N)
    let inline nextDouble() = fun (env: #HasRandom) -> ioOk(env.random.nextDouble())