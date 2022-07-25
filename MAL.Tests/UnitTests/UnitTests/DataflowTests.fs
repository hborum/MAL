module DataflowTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Dataflow
open ToIntermediate

[<TestClass>]
type TestClass () =
  [<TestMethod>]
  member __.simpleAddToGraph() =
    let statement = Let("x", (TBool, CstB false))
    let graph = (set [0], Set.empty, Map.add 0 (Block []) Map.empty)
    let expected = 1,(set [0;1], set [(1,0)], Map.ofList [(0,Block []);(1,statement)])
    let actual = extendGraph graph statement 0
    Assert.AreEqual(expected, actual)
  
  [<TestMethod>]
  member __.flowDependencyLet() =
    let write = Let("x", (TBool, CstB false))
    let read = Let("y", (TBool, Var "x"))
    let prog = [write;read]
    let expected : Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt> = (set [(write,read)], Set.empty, Set.empty)
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  [<TestMethod>]
  member __.antiDependencyLet() =
    let write = Let("x", (TBool, CstB false))
    let read = Let("y", (TBool, Var "x"))
    let prog = [read;write]
    let expected : Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt> = (Set.empty, set [(read, write)], Set.empty)
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  [<TestMethod>]
  member __.outputDependencyLet() =
    let write1 = Let("x", (TBool, CstB false))
    let write2 = Let("x", (TBool, CstB true))
    let prog = [write1; write2]
    let expected : Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt> = (Set.empty, Set.empty, set [(write1,write2)])
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  [<TestMethod>]
  member __.flowDependencyAssign() =
    let write = Assign("Global", ["x"], (TBool, CstB false))
    let read = Let("y", (TBool, Var "x"))
    let prog = [write;read]
    let expected : Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt> = (set [(write,read)], Set.empty, Set.empty)
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  [<TestMethod>]
  member __.antiDependencyAssign() =
    let write = Assign("Global", ["x"], (TBool, CstB false))
    let read = Let("y", (TBool, Var "x"))
    let prog = [read;write]
    let expected : Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt> = (Set.empty, set [(read, write)], Set.empty)
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)

  [<TestMethod>]
  member __.outputDependencyAssign() =
    let write1 = Assign("Global", ["x"], (TBool, CstB false))
    let write2 = Assign("Global", ["x"], (TBool, CstB true))
    let prog = [write1; write2]
    let expected : Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt>*Set<ILstmt*ILstmt> = (Set.empty, Set.empty, set [(write1,write2)])
    let actual = collectDependencies prog
    Assert.AreEqual(expected, actual)