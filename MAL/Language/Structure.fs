namespace itu.dk.MAL
open AST

module Structure =

  type modulKind<'sA> =
    | MainModul
    | ImportModul of modulSyntax<'sA> * identifier<'sA>

  type modul<'eA, 'sA> =
    modulKind<'sA> * dec<'eA, 'sA> list

  type dependencyKind =
    | DependAll
    | DependData

  [<StructuredFormatDisplay("{ASTStringWithTypes}")>]
  type program<'eA, 'sA> =
    { modul : modul<'eA, 'sA>
    ; source : string
    ; dependencies : (dependencyKind * identifier<'sA> * program<'eA, 'sA>) list
    }
    member this.ASTStringWithTypes =
      let decs = snd this.modul
      match box decs with
      | :? List<dec<typ<Position>, Position>> as decs -> Printer.printASTWithTypeInfo decs
      | _ -> Printer.printASTWithNoInfo decs

  type store<'a> = Map<storeId, 'a>

  type environment<'a> = (string * 'a) list
  type relation = (storeId * storeId) list