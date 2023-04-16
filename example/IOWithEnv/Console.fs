module IOWithEnv.Console

open System
open System.Runtime.CompilerServices
open RZ.FSharp.IO

type ConsoleIO =
    abstract member ReadLine: unit -> string
    abstract member Write: string -> unit
    abstract member WriteLine: string -> unit
    
type HasConsole =
    abstract member Console: ConsoleIO
        
[<IsReadOnly; Struct; NoComparison; NoEquality>]
type RealConsoleIO =
    static member Default = Unchecked.defaultof<RealConsoleIO>
    
    interface ConsoleIO with
        member _.ReadLine() = Console.ReadLine()
        member _.Write s    = Console.Write s
        member _.WriteLine s= Console.WriteLine s
        
let inline read_line()  = fun (rt: #HasConsole) -> ioOk(rt.Console.ReadLine() )
let inline write s      = fun (rt: #HasConsole) -> ioOk(rt.Console.Write s    )
let inline write_line s = fun (rt: #HasConsole) -> ioOk(rt.Console.WriteLine s)