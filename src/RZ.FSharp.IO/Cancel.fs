namespace RZ.FSharp.IO

#nowarn "760" // ignore using new

open System
open System.Threading

type Cancel =
    abstract member token: CancellationToken
    abstract member cancel: unit -> unit
    abstract member cancelAfter: TimeSpan -> unit
    abstract member createLocal: unit -> Cancel

type SupportCancel =
    abstract member cancel: Cancel
    
module Cancel =
    [<Struct; NoComparison; NoEquality>]
    type LocalCancel(ct: CancellationTokenSource) =
        static member createLocalTokenFrom(token: CancellationToken) = 
            let localSource = CancellationTokenSource()
            token.Register(fun() -> localSource.Cancel()) |> ignore
            LocalCancel(localSource)
            
        interface Cancel with
            member _.token = ct.Token
            member _.cancel() = ct.Cancel()
            member _.cancelAfter t = ct.CancelAfter t
            member _.createLocal() = LocalCancel.createLocalTokenFrom ct.Token
            
    let inline ``new``() :Cancel = LocalCancel(CancellationTokenSource())
    let inline link token :Cancel = LocalCancel.createLocalTokenFrom token