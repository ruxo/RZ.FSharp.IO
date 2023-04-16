open RZ.FSharp.IO
open IOWithEnv.Console
open IOWithEnv.Random

let play target =
    io {
        do! write "Guess: "
        let! guess = read_line().map(int)
        if guess = target then
            do! write_line $"Congrat, it's %d{guess}!"
        else
            do! if guess < target
                then write_line "Too small"
                else write_line "Too big"
            yield ioErr(exn "Incorrect guess")
    }

let program =
    io {
        let! target = random_next 10
        do! IO.retry(play target)
    }
    
[<Struct; NoComparison; NoEquality>]
type MyEnv =
    static member Default = Unchecked.defaultof<MyEnv>
    
    interface HasConsole with
        member _.Console = RealConsoleIO.Default
        
    interface HasRandom with
        member _.RandomEff = RealRandom.Default
        
program.run(MyEnv.Default).await()