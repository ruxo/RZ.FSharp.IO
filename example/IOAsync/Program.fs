open System
open RZ.FSharp.Extension
open RZ.FSharp.Extension.TimeSpan
open RZ.FSharp.IO
open RZ.FSharp.IO.Sys

let digit = io {
    do! Console.writeLine "*"
    return 1
}

let sum = digit.fold(Schedule.recurs(10) |||| Schedule.spaced(1*Seconds), 0, fun s x -> s + x)

let inner = io {
    let! x = sum
    do! Console.writeLine $"total: %d{x}"
}

let play = io {
    let! cancel = IO.fork inner
    let! _ = Console.readKey
    do! cancel
    do! Console.writeLine "done"
}

[<NoComparison; NoEquality>]
type Live(cancel: Cancel) =
    static member Default = Live(Cancel.``new``())
    
    interface HasConsole with member _.console = RealConsole.Default
    interface SupportCancel with member _.cancel = cancel
    interface HasLocalContext<Live> with member _.createLocal() = Live(cancel.createLocal())

play.run(Live.Default).await()