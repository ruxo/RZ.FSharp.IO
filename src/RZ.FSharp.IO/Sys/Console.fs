namespace RZ.FSharp.IO.Sys

open System
open System.Runtime.CompilerServices

type Console =
    abstract member readKey: unit -> ConsoleKeyInfo
    abstract member readLine: unit -> string
    abstract member write: string -> unit
    abstract member writeLine: string -> unit
    
type HasConsole =
    abstract member console: Console
    
[<IsReadOnly; Struct; NoComparison; NoEquality>]
type RealConsole =
    static member Default = RealConsole()
    
    interface Console with
        member _.readKey()   = System.Console.ReadKey()
        member _.readLine()  = System.Console.ReadLine()
        member _.write s     = System.Console.Write s
        member _.writeLine s = System.Console.WriteLine s
        
module Console =
    open RZ.FSharp.IO
    
    let readKey = fun (env: #HasConsole) -> ioOk(env.console.readKey())
    let readLine= fun (env: #HasConsole) -> ioOk(env.console.readLine())
    
    let inline write s     = fun (env: #HasConsole) -> ioOk(env.console.write s)
    let inline writeLine s = fun (env: #HasConsole) -> ioOk(env.console.writeLine s)