namespace itu.dk.MAL

module SyntaxUtil =

  open Microsoft.CodeAnalysis.CSharp.Syntax
  open System.Linq

  let getMethod (clas: MemberDeclarationSyntax) (name) =
    clas.DescendantNodes()
        .OfType<MethodDeclarationSyntax>()
        .First( fun methodSyntax -> methodSyntax.Identifier.Text = name)

  let getConstructor (clas: MemberDeclarationSyntax) =
    clas.DescendantNodes()
        .OfType<ConstructorDeclarationSyntax>().First()

  let getClass (clas: MemberDeclarationSyntax) =
    [clas].OfType<ClassDeclarationSyntax>()
          .First( fun methodSyntax -> true)

  type OptimizationLevel =
    | ToRelease
    | ToDebug

  type CompilerOptions =
    { parallelize : bool
    ; inln : bool //Currently unused
    // parallelize => inln. inln probably illegal at some point
    ; vectorize : bool
    ; deforest : bool
    ; outPath : string
    ; outName : string
    ; optLevel : OptimizationLevel
    }
    static member Default =
      { parallelize = false
      ; inln = false
      ; vectorize = false
      ; deforest = false
      ; outPath = "out/"
      ; outName = "Default"
      ; optLevel = ToDebug
      }