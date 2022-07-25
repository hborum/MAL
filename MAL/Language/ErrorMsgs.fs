namespace itu.dk.MAL

module ErrorMsgs =
  open AnalyserTypes
  open PositionBounds

  let addPos (f : 'a -> typError) (a : 'a) pos1 pos2 =
    let err = f a
    err, (posToIndex (pos1, 0), posToIndex (pos2, 0))

  // Module Import errors
  let import_not_found s  = sprintf "The imported module '%s' could not be located." s
  let import_module_error = addPos (fun s -> ModuleError <| sprintf "The imported module '%s' contains errors." s)
  let import_modul_unknown_data =
    addPos <|
      fun (modul, tag) ->
        ModuleError (sprintf "The imported module '%s' has a data definition for %s. This module must contain an equivalent definition" modul tag)

  let import_modul_data_inc =
    addPos <|
      fun (modul, tag, field, typ) ->
        ModuleError (sprintf "The imported module '%s' requires that the data definition for '%s' contains the field '%s : %s'." modul tag field typ)

  let import_modul_data_must_include =
    addPos <|
      fun (modul, tag, field, typ) ->
        ModuleError (sprintf "The module '%s' requires '%s' to contain the field '%s : %s'" modul tag field typ)

  // Initialise errors
  let module_not_initialised =
    addPos <|
      fun (m_name, actionOrFunction) ->
        UsageBeforeInit (sprintf "The module %s must be initialised before using any of its %s" m_name actionOrFunction)

  let module_does_not_init_provides =
    addPos <|
      fun m_name ->
        InitError (sprintf "The module %s does not initialise everything it states it provides" m_name)

  let field_not_init_by_init =
    addPos <|
      fun (field, tag) ->
        InitError (sprintf  "The field '%s' of '%s' must be initialised by the 'init'-action" field tag)

  let value_used_before_init =
    addPos <|
      fun (proj, tag) ->
        UsageBeforeInit <| sprintf "The value %s on %s is used before it is initialised" proj tag

  // Type errors
  // Todo: Concrete syntax in string
  let unkown_data_definition =
    addPos <|
      fun (tag) ->
        InvalidType <| sprintf "The data type %s is unkown. Try to adding the definition 'data %s with ... end'" tag tag

  let create_record_missing_field =
    addPos <|
      fun (tag, nm, typ) ->
        (BadArgs <| sprintf "%s must contain a %s : %s" tag nm typ)

  let must_have_field =
    addPos <|
      fun (nm, t, t') ->
        BadArgs <| sprintf "%s must be %s is a %s." nm t t'

  let data_does_not_contain =
    addPos <|
      fun (tag, nm) ->
        DoesNotContain <| sprintf "Data %s does not contain the field %s." tag nm

  let var_is_undefined =
    addPos <|
      fun (x) ->
        VarDoesNotExist <| sprintf "Variable %s is undefined." x

  let cant_filter_with =
    addPos <|
      fun (t1, tags : List<string>) ->
        NotSubTypeOf <| sprintf "Can't filter a %s with %A" t1 tags

  let operator_not_compatible =
    addPos <|
      fun (binOp, t1, t2) ->
        BadArgs <| sprintf "The operator '%s' was used with (%s %s %s)" binOp t1 binOp t2

  // Todo: Concrete syntax in string
  let if_expr_must_be_boolean =
    addPos <|
      fun (t1) ->
        (ConditionalNotBool <| sprintf "The conditional of an if-expression must be Boolean, is %s" t1)

  let if_expr_same_branches =
    addPos <|
      fun (t2, t3) ->
        (BranchesNotTheSame <| sprintf "The branches of an if-statement must be the same. Is %s and %s" t2 t3)

  let warning_match_useless =
    addPos <|
      fun (tag) ->
        Warning <| sprintf "Warning: You can not match on %s to a more specific type" tag

  let cannot_match =
    addPos <|
      fun (t1) ->
          NotRecord <| sprintf "You cannot match on a %s" t1

  let wrong_match_branches =
    addPos <|
      fun (matchPattern) ->
         UncoveredBranch <| sprintf "Match case can not be right. No or multiple cases for %s" matchPattern

  let expected_branch_for =
    addPos <|
      fun (pat) ->
        UncoveredBranch <| sprintf "Expected a branch for: %s" pat

  let different_branches =
    addPos <|
      fun (t1, t2) ->
        BranchesNotTheSame <| sprintf "The branches must be the same type. First is %s and this is %s" t1 t2

  let list_must_be_of_type =
    addPos <|
      fun (t1) ->
        ElementsNotTheSame <| sprintf "List must be of type %s" t1

  let elements_must_be_the_same =
    addPos <|
      fun (headTyp) ->
        ElementsNotTheSame <| sprintf "Expected all elements in list to have type %s" headTyp

  let not_callable =
    addPos <|
      fun (t1) ->
        NotCallable <| sprintf "Cannot call %s" t1

  let not_enumerable =
    addPos <|
      fun (loopName, t1) ->
        NotEnumarable <| sprintf "Cannot do %s on a %s" loopName t1

  // Todo: Concrete Syntax
  let where_must_be_bool =
    addPos <|
      fun (t1) ->
        ConditionalNotBool <| sprintf "A where clause must be a Boolean. Is a %s" t1

  let module_does_not_contain =
    addPos <|
      fun (nm,x) ->
        DoesNotContain <| sprintf "The module %s does not contain %s" nm x

  let expected_module_or_record =
    addPos <|
      fun (t1) ->
        NotRecord <| sprintf "Expected a record or a module got %s" t1

  let cannot_assign_to =
    addPos <|
      fun (t_ass, t_new) ->
        NotSame <| sprintf "Cannot assign a %s with a %s." t_ass t_new

  let cannot_assign_to_a_reserve =
    addPos <|
      fun () ->
        NotSame <| sprintf "Cannot assign to a Reserve, only transfer."

  let action_or_module_undefined =
    addPos <|
      fun (act_nm) ->
        VarDoesNotExist <| sprintf "The action or module %s is undefined." act_nm

  let the_module_does_not_export =
    addPos <|
      fun (x) ->
       DoesNotContain <| sprintf "The module does not export action %s." x

  let X_does_not_contain =
    addPos <|
      fun (t1,x ) ->
        DoesNotContain <| sprintf "%s does not contain `%s`" t1 x

  let action_expected =
    addPos <|
      fun (t1, t2) ->
        BadArgs <| sprintf "The action expected (%s) got (%s)" t1 t2

  let only_do_action =
    addPos <|
      fun () ->
        OnlyDoAction "It is only possible to do an action, not a function."

  // Todo: Concrete Syntax
  let can_only_overwrite_with =
    addPos <|
      fun (t_new) ->
        NotSame <| sprintf "Can only overwrite with a Float not a %s" t_new

  let can_only_overwrite_a =
    addPos <|
      fun (t_new) ->
        NotSame <| sprintf "Can only overwrite a Reserve not a(n) %s." t_new

  let can_only_transfer_float =
    addPos <|
      fun (t1) ->
        NotSame <| sprintf "can not only transfer an amount specified by a Float not a(n) %s." t1

  let can_only_transfer_from_r =
    addPos <|
      fun (t1) ->
        NotSame <| sprintf "can only transfer from a Reserve not a(n) %s." t1

  let can_only_transfer_to_r =
    addPos <|
      fun (t1) ->
        NotSame <| sprintf "can only transfer to a Reserve not a(n) %s." t1


  let the_type_is_not_valid =
    addPos <|
      fun (tags) ->
        InvalidType <| sprintf "%s is not a valid type" tags

  //let the_module_does_not_export =
  //  addPos <|
  //    fun () ->
  //    VarDoesNotExist ""

  //------------------------------
  //-- from ParserValidation.fs --
  //------------------------------
  let err_in = "Internal error. Please report this error."

  // "Did not expect the keyword ..." errors
  let err_kw = "Did not expect the keyword "

  // "Expected ..." (keyword) errors
  let err_expected_Map      = "Expected the keyword 'Map'" // doesn't happen
  let err_expected_la       = "Expected the keyword '<'"
  let err_expected_ra       = "Expected the keyword '>'"
  let err_expected_comma    = "Expected the keyword ','"

  let err_expected_map      = "Expected the keyword 'map'" // doesn't happen
  let err_expected_in       = "Expected the keyword 'in'"
  let err_expected_where    = "Expected the keyword 'where'"
  let err_expected_with     = "Expected the keyword 'with'"
  let err_expected_endmap   = "Expected the keyword 'end' for map-expression"

  let err_expected_let      = "Expected the keyword 'let'" // doesn't happen
  let err_expected_eq       = "Expected the keyword '='"
  let err_expected_endlet   = "Expected the keyword 'end' for let-expression"

  let err_expected_match    = "Expected the keyword 'match'" // doesn't happen
  let err_expected_endmatch = "Expected the keyword 'end' for match-expression"

  let err_expected_endupt   = "Expected the keyword 'end' for updates-expression"

  let err_expected_if       = "Expected the keyword 'if'" // doesn't happen
  let err_expected_then     = "Expected the keyword 'then'"
  let err_expected_else     = "Expected the keyword 'else'"

  let err_expected_end      = "Expected the keyword 'end'"

  let err_expected_kw_lpar  = "Expected the keyword '('"
  let err_expected_kw_rpar  = "Expected the keyword ')'"

  let err_expected_sep      = "Expected the keyword ';'"
  let err_expected_lbrck    = "Expected the keyword '['"
  let err_expected_rbrck    = "Expected the keyword ']'"

  let err_expected_dot      = "Expected the keyword '.'"
  let err_expected_colon    = "Expected the keyword ':'"

  // "Expected ..." (type) errors
  let err_expected_tpname = "Expected a type name"

  // "Expected ..." (other) errors
  let err_expected_expr = "Expected an expression"
  let err_expected_var  = "Expected variable name"
  let err_expected_lpar = "Expected ("

  let err_expected_bar = "Expected '|' in match expression"

  // Missing pattern error
  let err_missing_pattern = "Expected a valid match pattern. Either a tag, None, or Some. e.g. \"| Interest iGrp -> 2\""

  // Error with no position (when does this happen?)
  let err_no_pos = "Error produced with no position"

  //-------------------
  //-- from Typer.fs --
  //-------------------
  let err_eq typs                   = sprintf "= expects a (Any1, Any1) got %A" typs
  let err_nodef tag                 = sprintf "There is no data definition for %s.\ne.g. data %s" tag tag
  let err_must_contain tag nm       = sprintf "%s must contain a %s : %s" tag nm
  //let err_??? = sprintf "%s must be %s is a %s." nm
  let err_doesnotcontain tag nm     = sprintf "Data %s does not contain a %s." tag nm
  //are these the same?
  let err_doesnotcontain2 tag nm    = sprintf "Data %s does not contain the field %s." tag nm
  let err_var_undefined x           = sprintf "Variable %s is undefined." x
  let err_not_subtype t1 t2         = sprintf "Can't filter a %A with %A" t1 t2

  // BadArgs
  let err_badargs_binop binop t1 t2 = sprintf "The operator '%s' was used with (%s %s %s)" binop t1 binop t2
  let err_badargs f t1 t2           = sprintf "%s takes (%A) got (%A)" f t1 t2

  // conditionals
  let err_cond_bool t               = sprintf "The conditional of an if-statement must be Boolean, is %s" t
  let err_cond_branch t1 t2         = sprintf "The branches of an if-statement must be the same. Is %s and %s" t1 t2

  let err_data_not_extended t       = sprintf "Data %s is not extended by any other data" t

  let err_match1 t                   = sprintf "Can't match on a %s" t
  let err_match2 t                   = sprintf "Match case can not be right. No or multiple cases for %A" t
  let err_match_uncovered pat        = sprintf "Expected a branch for: %A" pat
  let err_match_same t1 t2           = sprintf "The branches must be the same type. First is %s and this is %s" t1 t2

  //lists
  let err_list_type t                = sprintf "Expected all elements in list to have type %s" t

  let err_notcallable t              = sprintf "Can't call %s" t

  let err_not_enumerable loop t      = sprintf "Can't do %s on a %s" loop t

  //where
  let err_where_bool t               = sprintf "A where clause must be a Bool. Is a %s" t //why do we have two of these?

  //projections
  let err_proj t                     = sprintf "Type error, expected record got %A" t

  //assignments
  let err_assign_type t1 t2          = sprintf "can not assign a %s with a %s." t1 t2
  let err_assign_res                 = sprintf "can not assign to a Reserve, only transfer."

  //actions
  let err_action t1 t2               = sprintf "the action takes (%A) got (%A)" t1 t2
  let err_only_action                = "It is only possible to do an action, not a function."
  let err_only_action2               = "It is only possible to do an action."

  //overwrite
  let err_overwrite_float t          = sprintf "can only overwrite with a Float not a(n) %s." t
  let err_overwrite_reserve t        = sprintf "can only overwrite an Reserve not a(n) %s." t

  //transfer
  let err_trans_float t              = sprintf "can not only transfer an amount specified by a Float not a(n) %s." t
  let err_trans_from t               = sprintf "can only transfer from a Reserve not a(n) %s." t
  let err_trans_to t                 = sprintf "can only transfer to a Reserve not a(n) %s." t

  let err_type_invalid t             = sprintf "%s is not a valid type" t

  //functions
  let err_fun_duplicate f            = sprintf "The function %s is defined twice" f
  let err_fun_cycle f                = sprintf "The function %s appears in a recursive cycle" f
  let err_act_cycle f                = sprintf "The action %s appears in a recursive cycle" f
  let err_no_rec_fun                 = "Can not find recursive function, should be impossible"
