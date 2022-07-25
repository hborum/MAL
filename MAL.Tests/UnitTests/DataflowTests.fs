module DataflowTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open itu.dk.MAL.Dataflow
open itu.dk.MAL.ToIntermediate
open System.Collections.Generic

let globX = (TBool, Access( (TRec ["Global"], Var "Global"), "x"))

let listToHashMap (xs : ('a * 'b) list) =
  let h = new HashSet<'a * 'b>()
  for (a,b) in xs do
    h.Add(a,b) |> ignore
  h

[<TestClass>]
type ControlGraphCreation () =
  let graph = (0, Set.empty, Map.add 0 (Block []) Map.empty)
  //[<TestMethod>]
  member __.simpleAddToGraph() =
    let statement = Let("x", (TBool, CstB false), true)
    let expected = (1, set [((1,0),(0,0))], Map.ofList [(0,Block []);(1,statement)])
    let actual = extendGraph graph statement 0 (0,0) |> snd
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.MatchToGraph() =
    let c = (TRec ["Cashflow"], Var "c")
    let assign = Assign(c, (TBool, CstB false))
    let case1 = (TRec ["WithExpenses"], "we", Expression (TInt, CstI 1))
    let case2 = (TRec ["WithoutExpenses"], "woe", Expression (TInt, CstI 2))
    let switch = Match((TRec ["Cashflow"], Var "c"), [case1; case2])
    printfn "%A" (extendGraph graph switch 0 (0,0) |> snd)

[<TestClass>]
type DependenceDetection () =
  //[<TestMethod>]
  member __.flowDependencyLet() =
    let write = Let("x", (TBool, CstB false), true)
    let read = Let("y", (TBool, Var "x"), true)
    let prog = [write;read]
    let rw = new HashSet<ILstmt*ILstmt>()
    rw.Add(write,read) |> ignore
    let expected : HashSet<ILstmt*ILstmt>*HashSet<ILstmt*ILstmt>*HashSet<ILstmt*ILstmt> = (rw, new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>())
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.antiDependencyLet() =
    let write = Let("x", (TBool, CstB false), true)
    let read = Let("y", (TBool, Var "x"), true)
    let prog = [read;write]
    let rw = new HashSet<ILstmt*ILstmt>()
    rw.Add(write,read) |> ignore
    let expected : HashSet<ILstmt*ILstmt>*HashSet<ILstmt*ILstmt>*HashSet<ILstmt*ILstmt> = (new HashSet<ILstmt*ILstmt>(), rw, new HashSet<ILstmt*ILstmt>())
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.outputDependencyLet() =
    let write1 = Let("x", (TBool, CstB false), true)
    let write2 = Let("x", (TBool, CstB true), true)
    let prog = [write1; write2]
    let ww = new HashSet<ILstmt*ILstmt>()
    ww.Add(write1, write2) |> ignore
    let expected  = (new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>(), ww)
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.flowDependencyAssign() =
    let x = (TBool, Access( (TRec ["Global"], Var "Global"), "x"))
    let write = Assign(x, (TBool, CstB false))
    let read = Let("y", (TBool, Access ((TRec ["Global"], Var "Global"), "x")), true)
    let prog = [write;read]
    let rw = new HashSet<ILstmt*ILstmt>()
    rw.Add(write,read) |> ignore
    let expected  = (rw, new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>())
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.antiDependencyAssign() =
    let write = Assign(globX, (TBool, CstB false))
    let read = Let("y", (TBool, Access ((TRec ["Global"], Var "Global"), "x")), true)
    let prog = [read;write]
    let rw = new HashSet<ILstmt*ILstmt>()
    rw.Add(write,read) |> ignore
    let expected  = (new HashSet<ILstmt*ILstmt>(), rw, new HashSet<ILstmt*ILstmt>())
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.outputDependencyAssign() =
    let write1 = Assign(globX, (TBool, CstB false))
    let write2 = Assign(globX, (TBool, CstB true))
    let prog = [write1; write2]
    let ww = new HashSet<ILstmt*ILstmt>()
    ww.Add(write1,write2) |> ignore
    let expected  = (new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>(), ww)
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.noDependencies() =
    let write1 = Assign(globX, (TBool, CstB false))
    let write2 = Assign((TBool, Access( (TRec ["Global"], Var "Global"), "y")), (TBool, CstB true))
    let write3 = Let("x", (TBool, CstB true), true)
    let prog = [write1; write2; write3]
    let expected  = (new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>())
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.doubleDependencies() =
    let read = Let("y", (TBool, Access ((TRec ["Global"], Var "Global"), "x")), true)
    let write = Assign(globX, (TBool, Var "y"))
    let prog = [read; write]
    let rw = new HashSet<ILstmt*ILstmt>()
    rw.Add(write,read) |> ignore
    let expected  = (rw, rw, new HashSet<ILstmt*ILstmt>())
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.multipleDependencies() =
    let write1 = Assign(globX, (TBool, CstB false))
    let read1 = Let("y_0", (TBool, Access ((TRec ["Global"], Var "Global"), "x")), true)
    let write2 = Assign(globX, (TBool, Var "y_0"))
    let write3 = Let("y_0", (TBool, CstB true), true)
    let prog = [write1; read1; write2; write3]

    let flows = new HashSet<ILstmt*ILstmt>()
    flows.Add(write1,read1) |> ignore
    flows.Add(read1,write2) |> ignore
    let antis = new HashSet<ILstmt*ILstmt>()
    antis.Add(read1,write2) |> ignore
    antis.Add(write2,write3) |> ignore
    let outputs = new HashSet<ILstmt*ILstmt>()
    outputs.Add(write1,write2) |> ignore
    outputs.Add(read1,write3) |> ignore

    let expected  = (flows, antis, outputs)
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.blockDependencies() =
    let write1 = Assign(globX, (TBool, CstB false))
    let read1 = Let("y_0", globX, true)
    let write2 = Assign(globX, (TBool, Var "y_0"))
    let write3 = Let("y_0", (TBool, CstB true), true)
    let prog = [Block [write1; read1; write2; write3]]
    let flows = listToHashMap [(write1,read1);(read1,write2)]
    let antis = listToHashMap [(read1, write2); (write2, write3)]
    let outputs = listToHashMap [(write1, write2); (read1, write3)]

    let expected  = (flows, antis, outputs)
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)
  ////[<TestMethod>]
  member __.noScopebreakingDependencies() =
    let binding = Let("y", (TInt, CstI 0), true)
    let tbranch = Let("y", (TInt, CstI 1), true)
    let fbranch = Let("z", (TInt, CstI 2), true)
    let scoped = If((TBool, CstB true), tbranch, fbranch)
    let ender = Let("x", (TInt, Var "y"), true)
    let prog = [binding; scoped; ender]
    let flows = listToHashMap [(binding, ender)]
    let antis = listToHashMap []
    let outputs = listToHashMap [(binding, tbranch)]
    let expected  = (flows, antis, outputs)
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  ////[<TestMethod>]
  member __.loopFlowDependency() =
    let read = Assign(globX, (TBool, Var "y"))
    let loop = For("y",["iterName_2", (TArray(TRec ["Cashflow"]),Var "Cashflows")], read, None)
    let prog = [loop]
    let flows = listToHashMap [(loop,read)]
    let antis = listToHashMap [(read, loop)]
    let outputs = listToHashMap [(read, read); (loop, loop)]
    let expected = (flows, antis, outputs)
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.loopOutputDependencyInteral() =
    let assign = Assign(globX, (TBool, CstB false))
    let loop = For("q",["iterName_2", (TArray(TRec ["Cashflow"]),Var "Cashflows")], assign, None)
    let depend = listToHashMap [(assign, assign); (loop, loop)]
    let expected  = (new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>(), depend)
    let actual = collectDependencies [loop]
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.loopOutputDependencyExternal() =
    let outerAssign = Assign(globX, (TBool, CstB true))
    let innerAssign = Assign(globX, (TBool, CstB false))
    let loop = For("q",["iterName_2", (TArray(TRec ["Cashflow"]),Var "Cashflows")], innerAssign, None)
    let depend = listToHashMap [(outerAssign, innerAssign); (innerAssign, innerAssign); (loop, loop)]
    let expected  = (new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>(), depend)
    let actual = collectDependencies [outerAssign; loop]
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.loopAllDependencies() =
    let assign = Assign(globX, globX)
    let assignAfter = Assign(globX, (TBool, CstB false))
    let loop = For("q",["iterName_2", (TArray(TRec ["Cashflow"]),Var "Cashflows")], assign, None)
    let flow = listToHashMap [(assign, assign)]
    let anti = listToHashMap <| (assign, assignAfter)::[(assign, assign)]
    let output = listToHashMap <| (loop, loop)::(assign, assignAfter)::[(assign, assign)]
    let expected = (flow, anti, output)
    let actual = collectDependencies [loop; assignAfter]
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.callFlowDependency() =
    let assign = Assign(globX, (TBool, CstB true))
    let call = Expression(TUnit, (Call(Other, (TFun IFun, Var "dummyfun"), [(TBool,Access(globX, "Get"))])))
    let loop = For("q",["iterName_2",(TArray(TRec ["Cashflow"]),Var "Cashflows")], call, None)
    let flow = listToHashMap [(assign, call)]
    let output = listToHashMap [(loop, loop)]
    let expected  = (flow, new HashSet<ILstmt*ILstmt>(), output)
    let actual = collectDependencies [assign; loop]
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.matchOutDependency() =
    let assign = Assign(globX, (TBool, CstB false))
    let over =  Assign(globX, (TBool, CstB true))
    let case1 = (TRec ["WithExpenses"], "we", over)
    let case2 = (TRec ["WithoutExpenses"], "woe", over)
    let switch = Match((TRec ["Cashflow"], Var "c"), [case1; case2])
    let output = listToHashMap [(assign, over)]
    let expected  = (new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>(), output)
    let actual = collectDependencies [assign; switch]
    Assert.AreEqual(expected, actual)

  ////[<TestMethod>]
  //member __.matchFlowDependency() =
  //  let assign = Assign(globX, (TBool, CstB false))
  //  let over =  Assign(globX, (TBool, CstB true))
  //  let case1 = (TRec ["WithExpenses"], "we", over)
  //  let case2 = (TRec ["WithoutExpenses"], "woe", over)
  //  let switch = Match((TRec ["Cashflow"], Var "c"), [case1; case2])
  //  let output = set [(assign, over);]
  //  let expected  = (new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>(), output)
  //  let actual = collectDependencies [assign; switch]
  //  Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.matchFlowDependency() =
    let matchassign (t, n, _) main = Let(n, main, true)
    let c = (TRec ["Cashflow"], Var "c")
    let assign = Assign(c, (TBool, CstB false))
    let case1 = (TRec ["WithExpenses"], "we", Expression (TInt, CstI 1))
    let case2 = (TRec ["WithoutExpenses"], "woe", Expression (TInt, CstI 2))
    let switch = Match(c, [case1; case2])
    let flow = listToHashMap [(assign, Expression c);(assign, matchassign case1 c); (assign, matchassign case2 c)]
    let expected  = (flow, new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>())
    let actual = collectDependencies [assign; switch]
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.readFromMutable() =
    let assign = Assign(globX, (TRec ["Cashflow"], (Access((TRec ["Global"], Var "Global"), "y"))))
    // Global.x = Global.y
    let read =
      Let("z",
        (TBool,
          (Access(
            (TRec ["Cashflow"], (Access((TRec ["Global"], Var "Global"), "x"))), "foo"))), true)
            // z = Global.x.foo
    let prog = [assign; read]
    let depend = listToHashMap [(assign,read)]
    let expected  = (depend, new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>())
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  ////[<TestMethod>]
  member __.subObjectDependency() =
    let assign = Assign(globX, (TRec ["Cashflow"], (Access((TRec ["Global"], Var "Global"), "foo"))))
    let read =  Assign((TBool,Access(globX,"y")), (TBool, CstI 2))
    let prog = [assign; read]
    let depend = listToHashMap [(assign,read)]
    let expected  = (depend, new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>())
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.doesNotReadFromSelf() =
    let stmtLet = Let("x", (TBool, (Var "x")), true)
    let stmtAssign = Assign(globX, (TRec ["Cashflow"], (Access((TRec ["Global"], Var "Global"), "x"))))
    let expected  = (new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>(), new HashSet<ILstmt*ILstmt>())
    let actual = collectDependencies [stmtLet; stmtAssign]
    Assert.AreEqual(expected, actual)

  //[<TestMethod>]
  member __.dependenciesCanCrossScope() =
    let stmtLet = Let("x", (TBool, (Var "x")), true)
    let stmtAssign = Assign((TBool, Var "z"), (TBool, Var "x"))
    let stmtFor = For("y",["iterName_2", (TArray (TBool), Var "Ys")], stmtAssign, None)
    let expected  = (listToHashMap [(stmtLet,stmtAssign)], new HashSet<ILstmt*ILstmt>(), listToHashMap[(stmtAssign, stmtAssign) ; (stmtFor, stmtFor)])
    let actual = collectDependencies [stmtLet; stmtFor]
    Assert.AreEqual(expected, actual)

[<TestClass>]
type ParallelDetection () =
  let qx = (TBool, Access( (TRec ["CashFlow"], Var "q"), "x"))
  //[<TestMethod>]
  member __.emptyLoopValidParallel() =
    let loop = For("q", ["iterName_2",(TArray(TRec ["Cashflow"]),Var "Cashflows")], Block [], None)
    Assert.IsTrue(canBeParallelized (collectDependencies [loop]) loop)

  //[<TestMethod>]
  member __.readFromMutableLoopInValidParallel() =
    let init = Assign(qx, (TInt, CstI 1))
    let assign = Let("x", (TRec ["Cashflow"], Var "q"), true)
    let ex = Expression((TRec ["Cashflow"], Var "x"))
    let loop = For("q", ["itername_2", (TArray(TRec ["Cashflow"]),Var "Cashflows")], Block [assign;ex], None)
    canBeParallelized (collectDependencies [init; loop]) loop |> ignore
    ()
    //Assert.IsFalse(canBeParallelized (collectDependencies [init; loop]) loop)

  ////[<TestMethod>]
  member __.writeToIterationLoopValidParallel() =
    let assign = Assign(qx, (TBool, CstB false))
    let loop = For("q", ["iterName_2",(TArray(TRec ["Cashflow"]),Var "Cashflows")], assign, None)
    Assert.IsTrue(canBeParallelized (collectDependencies [loop]) loop)

  //[<TestMethod>]
  member __.writeToVariableOutsideLoopInvalidParallel() =
    let assign = Assign(globX, (TInt, Access((TRec ["Cashflow"], Var "q"), "x")))
    let loop = For("q", ["iterName_2",(TArray(TRec ["Cashflow"]),Var "Cashflows")], assign, None)
    Assert.IsFalse(canBeParallelized (collectDependencies [loop]) loop)

  //[<TestMethod>]
  member __.creatingListInvalidParallel() =
    let listTyp = TArray(TRec ["Cashflow"])
    let init = Let("list", (listTyp, Init(listTyp, [])), false)
    let elem = (TDouble, Access ((TRec ["Cashflow"], Var "q"),"val"))
    let add = Call(Other,(TFun (SysFun (TDouble,TUnit)), Access((listTyp, Var "list"),"Add")), [elem])
    let loop = For("q", ["iterName_2",(TArray(TRec ["Cashflow"]),Var "Cashflows")], Expression (TUnit, add), None)
    Assert.IsFalse(canBeParallelized (collectDependencies [init;loop]) loop)

  //[<TestMethod>]
  member __.creatingNestedListInvalidParallel() =
    let listTyp = TArray(TRec ["Cashflow"])
    let init = Let("list", (listTyp, Init(listTyp, [])), false)
    let elem = (TDouble, Access ((TRec ["Cashflow"], Var "q"),"val"))
    let add = Call(Other,(TFun (SysFun (TDouble,TUnit)), Access((listTyp, Var "list"),"Add")), [elem])
    let loop = For("q", ["iterName_2",(TArray(TRec ["Cashflow"]),Var "Cashflows")], Block [Expression (TUnit, add)], None)
    Assert.IsFalse(canBeParallelized (collectDependencies [init;loop]) loop)

  ////[<TestMethod>]
  member __.innerLoopInvalidParallel() =
    let cfTyp = TRec ["Cashflow"]
    let list = TArray(TRec ["Cashflow"]), Access((TRec ["ExecutorState"], Var "state"), "Cashflows")
    let innerStmt = Assign((TInt, Access((cfTyp, Var "j"), "x")), (TInt, CstI 1))
    let outerStmt = Assign((TInt, Access((cfTyp, Var "i"), "x")), (TInt, CstI 2))
    let innerLoop = For("j", ["iterName_2",list], innerStmt, None)
    let outerLoop = For("i", ["iterName_2",list], Block [outerStmt; innerLoop], None)
    let deps = collectDependencies [outerLoop]
    printfn "%A" deps
    Assert.IsTrue(canBeParallelized deps innerLoop)
    //Might actually be parallel?
    Assert.IsFalse(canBeParallelized deps outerLoop)

  //[<TestMethod>]
  member __.actualFMALcode() =
    let deps = collectDependencies DataflowData.largeInit
    printfn "%A" deps
    Assert.IsFalse(canBeParallelized deps DataflowData.troublesomeLoop)

  ////[<TestMethod>]
  member __.matchParallel() =
    let case1 = (TRec ["WithExpenses"], "we", Expression (TDouble, Access ((TRec ["WithExpenses"] , Var "we"), "Reserve")))
    let assBlock =
      Block [
        Assign((TDouble,  Access ((TRec ["WithoutExpenses"] , Var "woe"),"Reserve2")) , (TDouble, CstD 2.2) )
      ; Assign((TDouble,  Access ((TRec ["WithoutExpenses"] , Var "woe"),"Reserve")) , (TDouble,  Access ((TRec ["WithoutExpenses"] , Var "woe"),"Reserve2")) )
      ]
    let case2 = (TRec ["WithoutExpenses"], "woe", assBlock )
    let switch = Match((TRec ["Cashflow"], Var "q"), [case1; case2])
    let loop = For("q", ["iterName_2",(TArray(TRec ["Cashflow"]),Var "Cashflows")], switch, None)
    let deps = collectDependencies [loop]
    Assert.IsTrue(canBeParallelized deps loop)


  ////[<TestMethod>]
  member __.doubleNest() =
    let polTyp = TRec ["Policy"]
    let ass = Assign((TDouble, Access ((polTyp , Var "p2"), "Res")), (TDouble, Access ((polTyp , Var "p1"), "Res")))

    let loop2 = For("p2", ["iterName_2",(TArray(polTyp),Var "Policies")], ass, None)
    let loop1 = For("p1", ["iterName_2",(TArray(polTyp),Var "Policies")], loop2, None)

    let deps = collectDependencies [loop1]
    Assert.IsTrue(canBeParallelized deps loop2)
    Assert.IsFalse(canBeParallelized deps loop1)