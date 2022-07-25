namespace itu.dk.MAL

module AnalyserTypes =

  type typError =
    | NoInfo of string
    | InternalError of string
    | BadArgs of string
    | VarDoesNotExist of string
    | DoesNotContain of string
    | OnlyDoAction of string
    | NotRecord of string
    | ConditionalNotBool of string
    | BranchesNotTheSame of string
    | ElementsNotTheSame of string
    | NotSubTypeOf of string
    | WrongArity of string
    | NotCallable of string
    | UncoveredBranch of string
    | NotEnumarable of string
    | NotSame of string
    | Warning of string
    | InvalidType of string
    | DuplicateFunction of string
    | RecursiveFunction of string
    | BadInheritance of string
    | ModuleError of string
    | UsageBeforeInit of string
    | InitError of string

  /// int * int is start and end index of error. This is converted to start index and length by
  type typErr = (typError * (int * int))

  let getMsgFromErrType typ =
      match typ with
      | VarDoesNotExist s
      | DoesNotContain s
      | NoInfo s
      | InternalError s
      | BadArgs s
      | OnlyDoAction s
      | NotRecord s
      | ConditionalNotBool s
      | BranchesNotTheSame s
      | ElementsNotTheSame s
      | NotSubTypeOf s
      | WrongArity s
      | NotCallable s
      | UncoveredBranch s
      | NotEnumarable s
      | NotSame s
      | Warning s
      | DuplicateFunction s
      | RecursiveFunction s
      | InvalidType s
      | BadInheritance s
      | ModuleError s
      | InitError s
      | UsageBeforeInit s -> s

  let noErr pos = (NoInfo "",pos)

