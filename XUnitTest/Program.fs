

module Program =
  open Generators
  open FsCheck
  open FsCheck.Random
  open Tests

  let [<EntryPoint>] main _ =
    let t = FsCheck.Gen.sample 1 1 <| MALGenerator.Program().Generator //forces registration of the mal generator
    //printfn "%A" t
    let config = {Config.Default with StartSize = 3; EndSize = 15; MaxTest = 100; Replay = None }
    //Check.One(config, Tests.intentionalTypeErrorsAreDetected)
    Check.All (config, typeof<MalProperties>.DeclaringType)
    0