open Types
open Typed
open Prog
(* open Type *)
(* open Argument *)
open Expression
open Declaration
open Statement

module Visitors = struct
(* TODO decide what's publicly visible *)

(*
TODO Annotation
They can contain arguments, which can contain expressions
However, they are not directly recursive
*)

(*
TODO Type is recursive
These can contain expressions
*)

(*
type ('a, 'b) type_visitor = {
  visit_bool: 'a -> 'b;
  visit_error: 'a -> 'b;
  visit_integer: 'a -> 'b;
  visit_int_type: 'a -> Expression.t -> 'b;
  visit_bit_type: 'a -> Expression.t -> 'b;
  visit_var_bit: 'a -> Expression.t -> 'b;
  visit_top_level_type: 'a -> P4String.t -> 'b;
  visit_type_name: 'a -> P4String.t -> 'b;
  (* TODO how to handle this case? *)
  enter_specialized_type_nil: 'a -> 'a;
  exit_specialized_type_nil: 'b -> 'b;
  enter_specialized_type_cons: 'a -> ('a * 'a);
  exit_specialized_type_cons: 'b -> 'b -> 'b;
  (* TODO side information can be recursive *)
  enter_header_stack: 'a -> Expression.t -> 'a;
  exit_header_stack: 'b -> 'b;
  visit_tuple_nil: 'a -> 'b;
  enter_tuple_cons: 'a -> ('a * 'a);
  exit_tuple_cons: 'b -> 'b -> 'b;
  visit_string: 'a -> 'b;
  visit_void: 'a -> 'b;
  visit_dont_care: 'a -> 'b;
}
*)

(*
let rec type_visit_helper v acc t_info =
  let dummy_info = fst t_info in
  match snd t_info with
  | Bool -> v.visit_bool acc
  | Error -> v.visit_error acc
  | Integer -> v.visit_integer acc
  | IntType e -> v.visit_int_type acc e
  | BitType e -> v.visit_bit_type acc e
  | VarBit e -> v.visit_var_bit acc e
  | TopLevelType s -> v.visit_top_level_type acc s
  | TypeName s -> v.visit_type_name acc s
  | SpecializedType {base; args} -> begin match args with
    | [] -> let acc' = v.enter_specialized_type_nil acc in
      v.exit_specialized_type_nil (type_visit_helper v acc' base)
    | h :: t -> let (acc_h, acc_t) = v.enter_specialized_type_cons acc in
      v.exit_specialized_type_cons
        (type_visit_helper v acc_h h)
        (type_visit_helper v acc_t
          (dummy_info, SpecializedType {base = base; args = t}))
    end
  | HeaderStack {header; size} ->
    let acc' = v.enter_header_stack acc size in
    v.exit_header_stack (type_visit_helper v acc' header)
  | Tuple l -> begin match l with
    | [] -> v.visit_tuple_nil acc
    | h :: t -> let (acc_h, acc_t) = v.enter_tuple_cons acc in
      v.exit_tuple_cons (type_visit_helper v acc_h h)
                        (type_visit_helper v acc_t (dummy_info, Tuple t))
    end
  | String -> v.visit_string acc
  | Void -> v.visit_void acc
  | DontCare -> v.visit_dont_care acc
*)

(*
TODO MethodPrototype
They are not directly recursive; they can appear in declarations
Statements can contain declarations
Block and Parser can contain statements
Statements and declarations can contain blocks
Declarations can contain parsers
This means that there's no cycle back to MethodPrototype
*)

type ('a, 'b) key_value_visitor = {
  visit_key_value: 'a -> P4String.t -> Expression.t -> 'b;
}

let key_value_visit_helper v acc (kv_info: Prog.KeyValue.t) =
  let kv = snd kv_info in
  v.visit_key_value acc kv.key kv.value

type ('a, 'b) expression_visitor = {
  visit_true: 'a -> 'b;
  visit_false: 'a -> 'b;
  visit_int: 'a -> P4Int.t -> 'b;
  visit_string: 'a -> P4String.t -> 'b;
  visit_name: 'a -> P4String.t -> 'b;
  visit_top_level: 'a -> P4String.t -> 'b;
  enter_array_access: 'a -> ('a * 'a);
  exit_array_access: 'b -> 'b -> 'b;
  enter_bit_string_access: 'a -> Util.bigint -> Util.bigint -> 'a;
  exit_bit_string_access: 'b -> 'b;
  visit_list_nil: 'a -> 'b;
  enter_list_cons: 'a -> ('a * 'a);
  exit_list_cons: 'b -> 'b -> 'b;
  visit_record: 'a ->  KeyValue.t list -> 'b;
  enter_unary_op: 'a -> Op.uni -> 'a;
  exit_unary_op: 'b -> 'b;
  enter_binary_op: 'a -> Op.bin -> ('a * 'a);
  exit_binary_op: 'b -> 'b -> 'b;
  enter_cast: 'a -> Type.t -> 'a;
  exit_cast: 'b -> 'b;
  visit_type_member: 'a -> Type.t -> P4String.t -> 'b;
  visit_error_member: 'a -> P4String.t -> 'b;
  enter_expression_member: 'a -> P4String.t -> 'a;
  exit_expression_member: 'b -> 'b;
  enter_ternary: 'a -> ('a * 'a * 'a);
  exit_ternary: 'b -> 'b -> 'b -> 'b;
  enter_function_call_nil: 'a -> Type.t list -> 'a;
  exit_function_call_nil: 'b -> 'b;
  enter_function_call_cons: 'a -> ('a * 'a);
  exit_function_call_cons: 'b option -> 'b -> 'b;
  visit_nameless_instantiation_nil: 'a -> Type.t -> 'b;
  enter_nameless_instantiation_cons: 'a -> ('a * 'a);
  exit_nameless_instantiation_cons: 'b -> 'b -> 'b;
  visit_dont_care: 'a -> 'b;
  enter_mask: 'a -> ('a * 'a);
  exit_mask: 'b -> 'b -> 'b;
  enter_range: 'a -> ('a * 'a);
  exit_range: 'b -> 'b -> 'b;
}

(* TODO Currently throws out type in typed_t *)
let rec expression_visit_helper v acc (e_info: Prog.Expression.t) =
  let dummy_info = fst e_info in
  let e_typ = snd e_info in
  match e_typ.expr with
  | True -> v.visit_true acc
  | False -> v.visit_false acc
  | Int i -> v.visit_int acc i
  | String s -> v.visit_string acc s
  | Name s -> v.visit_name acc s
  | TopLevel s -> v.visit_top_level acc s
  | ArrayAccess {array; index} ->
    let (acc_array, acc_index) = v.enter_array_access acc in
    v.exit_array_access (expression_visit_helper v acc_array array)
                        (expression_visit_helper v acc_index index)
  | BitStringAccess {bits; lo; hi} ->
    let acc' = v.enter_bit_string_access acc lo hi in
    v.exit_bit_string_access (expression_visit_helper v acc' bits)
  | List {values} -> begin match values with
    | [] -> v.visit_list_nil acc
    | h :: t -> let (acc_h, acc_t) = v.enter_list_cons acc in
      v.exit_list_cons
        (expression_visit_helper v acc_h h)
        (expression_visit_helper v acc_t
          (dummy_info, {e_typ with expr = List {values = t}}))
      end
  | Record {entries} -> v.visit_record acc entries
  | UnaryOp {op; arg} ->
    let acc' = v.enter_unary_op acc op in
    v.exit_unary_op (expression_visit_helper v acc' arg)
  | BinaryOp {op; args} ->
    let (acc1, acc2) = v.enter_binary_op acc op in
    v.exit_binary_op (expression_visit_helper v acc1 (fst args))
                     (expression_visit_helper v acc2 (snd args))
  | Cast {typ; expr} ->
    let acc' = v.enter_cast acc typ in
    v.exit_cast (expression_visit_helper v acc' expr)
  | TypeMember {typ; name} -> v.visit_type_member acc typ name
  | ErrorMember s -> v.visit_error_member acc s
  | ExpressionMember {expr; name} ->
    let acc' = v.enter_expression_member acc name in
    v.exit_expression_member (expression_visit_helper v acc' expr)
  | Ternary {cond; tru; fls} ->
    let (acc_cond, acc_tru, acc_fls) = v.enter_ternary acc in
    v.exit_ternary (expression_visit_helper v acc_cond cond)
                   (expression_visit_helper v acc_tru tru)
                   (expression_visit_helper v acc_fls fls)
  | FunctionCall {func; type_args; args} ->
    begin match args with
      | [] -> let acc' = v.enter_function_call_nil acc type_args in
        v.exit_function_call_nil (expression_visit_helper v acc' func)
      | None :: t -> let (acc_h, acc_t) = v.enter_function_call_cons acc in
        v.exit_function_call_cons
          None
          (expression_visit_helper v acc_t
            (dummy_info, {e_typ with
              expr = FunctionCall {
                func = func;
                type_args = type_args;
              args = t}}))
      | (Some h) :: t -> let (acc_h, acc_t) = v.enter_function_call_cons acc in
        v.exit_function_call_cons
          (Some (expression_visit_helper v acc_h h))
          (expression_visit_helper v acc_t
            (dummy_info, {e_typ with
              expr = FunctionCall {
                func = func;
                type_args = type_args;
                args = t;
              }}))
    end
  | NamelessInstantiation {typ; args} ->
    begin match args with
      | [] -> v.visit_nameless_instantiation_nil acc typ
      | h :: t ->
        let (acc_h, acc_t) = v.enter_nameless_instantiation_cons acc in
        v.exit_nameless_instantiation_cons
          (expression_visit_helper v acc_h h)
          (expression_visit_helper v acc_t
            (dummy_info, {e_typ with
              expr = NamelessInstantiation {
                typ = typ;
                args = t;
              }}))
    end
    (* v.visit_nameless_instantiation acc typ args *)
  | DontCare -> v.visit_dont_care acc
  | Mask {expr; mask} ->
    let (acc_expr, acc_mask) = v.enter_mask acc in
    v.exit_mask (expression_visit_helper v acc_expr expr)
                (expression_visit_helper v acc_mask mask)
  | Range {lo; hi} ->
    let (acc_lo, acc_hi) = v.enter_range acc in
    v.exit_range (expression_visit_helper v acc_lo lo)
                 (expression_visit_helper v acc_hi hi)

(*
TODO Table not recursive
Only one use elsewhere in types, might not need anything
*)

(*
TODO Match not recursive
Uses elsewhere are in Table and Parser
*)

(*
TODO Parser not recursive
One use elsewhere, it's in Declaration
The side information for it is left out for now
*)

type ('a, 'b) declaration_visitor = {
  visit_constant:
    'a -> Annotation.t list -> Type.t -> P4String.t -> Value.value -> 'b;
  visit_instantiation:
    'a -> Annotation.t list -> Type.t -> Expression.t list ->
    P4String.t -> Block.t option -> 'b;
  (* TODO current implementation handles side info only in nil case *)
  visit_parser_nil:
    'a -> Annotation.t list -> P4String.t ->
    P4String.t list -> TypeParameter.t list ->
    TypeParameter.t list -> Parser.state list -> 'b;
  enter_parser_cons: 'a -> ('a * 'a);
  exit_parser_cons: 'b -> 'b -> 'b;
  visit_control_nil:
    'a -> Annotation.t list -> P4String.t ->
    P4String.t list -> TypeParameter.t list ->
    TypeParameter.t list -> Block.t -> 'b;
  enter_control_cons: 'a -> ('a * 'a);
  exit_control_cons: 'b -> 'b -> 'b;
  visit_function:
    'a -> Type.t -> P4String.t -> P4String.t list ->
    TypeParameter.t list -> Block.t -> 'b;
  visit_extern_function:
    'a -> Annotation.t list -> Type.t -> P4String.t ->
    P4String.t list -> TypeParameter.t list -> 'b;
  visit_variable:
    'a -> Annotation.t list -> Type.t ->
    P4String.t -> Expression.t option -> 'b;
  visit_value_set:
    'a -> Annotation.t list -> Type.t ->
    Expression.t -> P4String.t -> 'b;
  visit_action:
    'a -> Annotation.t list -> P4String.t ->
    TypeParameter.t list -> TypeParameter.t list ->
    Block.t -> 'b;
  visit_table:
    'a -> Annotation.t list -> P4String.t ->
    Table.key list -> Table.action_ref list ->
    (Table.entry list) option ->
    Table.action_ref option ->
    P4Int.t option -> Table.property list -> 'b;
  visit_header:
    'a -> Annotation.t list -> P4String.t ->
    Declaration.field list -> 'b;
  visit_header_union:
    'a -> Annotation.t list -> P4String.t ->
    Declaration.field list -> 'b;
  visit_struct:
    'a -> Annotation.t list -> P4String.t ->
    Declaration.field list -> 'b;
  visit_error: 'a -> P4String.t list -> 'b;
  visit_match_kind: 'a -> P4String.t list -> 'b;
  visit_enum: 'a -> Annotation.t list -> P4String.t -> P4String.t list -> 'b;
  visit_serializable_enum:
    'a -> Annotation.t list -> Type.t -> P4String.t ->
    (P4String.t * Expression.t) list -> 'b;
  visit_extern_object:
    'a -> Annotation.t list -> P4String.t ->
    P4String.t list -> MethodPrototype.t list -> 'b;
  visit_type_def_type: 'a -> Annotation.t list -> P4String.t -> Type.t -> 'b;
  enter_type_def_decl: 'a -> Annotation.t list -> P4String.t -> 'a;
  exit_type_def_decl: 'b -> 'b;
  visit_new_type_type: 'a -> Annotation.t list -> P4String.t -> Type.t -> 'b;
  enter_new_type_decl: 'a -> Annotation.t list -> P4String.t -> 'a;
  exit_new_type_decl: 'b -> 'b;
  visit_control_type:
    'a -> Annotation.t list -> P4String.t ->
    P4String.t list -> TypeParameter.t list -> 'b;
  visit_parser_type:
    'a -> Annotation.t list -> P4String.t ->
    P4String.t list -> TypeParameter.t list -> 'b;
  visit_package_type:
    'a -> Annotation.t list -> P4String.t ->
    P4String.t list -> TypeParameter.t list -> 'b;
}

let rec declaration_visit_helper v acc (d_info: Prog.Declaration.t) =
  let dummy_info = fst d_info in
  match snd d_info with
  | Constant {annotations; typ; name; value} ->
    v.visit_constant acc annotations typ name value
  | Instantiation {annotations; typ; args; name; init} ->
    v.visit_instantiation acc annotations typ args name init
  | Parser {annotations; name; type_params;
    params; constructor_params; locals; states} ->
    begin match locals with
      | [] -> v.visit_parser_nil acc annotations name
        type_params params constructor_params states
      | h :: t -> let (acc_h, acc_t) = v.enter_parser_cons acc in
        v.exit_parser_cons
          (declaration_visit_helper v acc_h h)
          (declaration_visit_helper v acc_t
            (dummy_info, Parser {
              annotations = annotations;
              name = name;
              type_params = type_params;
              params = params;
              constructor_params = constructor_params;
              locals = t;
              states = states;
            }))
    end
  | Control {annotations; name; type_params;
    params; constructor_params; locals; apply} ->
    begin match locals with
      | [] -> v.visit_control_nil acc annotations name
        type_params params constructor_params apply
      | h :: t -> let (acc_h, acc_t) = v.enter_control_cons acc in
        v.exit_control_cons
          (declaration_visit_helper v acc_h h)
          (declaration_visit_helper v acc_t
          (dummy_info, Control {
            annotations = annotations;
            name = name;
            type_params = type_params;
            params = params;
            constructor_params = constructor_params;
            locals = t;
            apply = apply;
          }))
    end
  | Function {return; name; type_params; params; body} ->
    v.visit_function acc return name type_params params body
  | ExternFunction {annotations; return; name; type_params; params} ->
    v.visit_extern_function acc annotations return name type_params params
  | Variable {annotations; typ; name; init} ->
    v.visit_variable acc annotations typ name init
  | ValueSet {annotations; typ; size; name} ->
    v.visit_value_set acc annotations typ size name
  | Action {annotations; name; data_params; ctrl_params; body} ->
    v.visit_action acc annotations name data_params ctrl_params body
  | Table {annotations; name; key; actions; entries;
    default_action; size; custom_properties} ->
    v.visit_table acc annotations name key actions
    entries default_action size custom_properties
  | Header {annotations; name; fields} ->
    v.visit_header acc annotations name fields
  | HeaderUnion {annotations; name; fields} ->
    v.visit_header_union acc annotations name fields
  | Struct {annotations; name; fields} ->
    v.visit_struct acc annotations name fields
  | Error {members} -> v.visit_error acc members
  | MatchKind {members} -> v.visit_match_kind acc members
  | Enum {annotations; name; members} ->
    v.visit_enum acc annotations name members
  | SerializableEnum {annotations; typ; name; members} ->
    v.visit_serializable_enum acc annotations typ name members
  | ExternObject {annotations; name; type_params; methods} ->
    v.visit_extern_object acc annotations name type_params methods
  | TypeDef {annotations; name; typ_or_decl} ->
    begin match typ_or_decl with
      | Left typ -> v.visit_type_def_type acc annotations name typ
      | Right decl ->
        let acc' = v.enter_type_def_decl acc annotations name in
        let out' = declaration_visit_helper v acc' decl in
        v.exit_type_def_decl out'
    end
  | NewType {annotations; name; typ_or_decl} ->
    begin match typ_or_decl with
      | Left typ -> v.visit_new_type_type acc annotations name typ
      | Right decl ->
        let acc' = v.enter_new_type_decl acc annotations name in
        let out' = declaration_visit_helper v acc' decl in
        v.exit_new_type_decl out'
    end
  | ControlType { annotations; name; type_params; params} ->
    v.visit_control_type acc annotations name type_params params
  | ParserType { annotations; name; type_params; params} ->
    v.visit_parser_type acc annotations name type_params params
  | PackageType { annotations; name; type_params; params} ->
    v.visit_package_type acc annotations name type_params params

type ('a, 'b) statement_visitor = {
  visit_method_call:
    'a -> Expression.t -> Type.t list -> (Expression.t option) list -> 'b;
  visit_assignment: 'a -> Expression.t -> Expression.t -> 'b;
  visit_direct_application: 'a -> Type.t -> Expression.t list -> 'b;
  (* the right member of the output is unused in the None case *)
  enter_conditional: 'a -> Expression.t -> ('a * 'a);
  exit_conditional: 'b -> 'b option -> 'b;
  (* visit_block_statement: 'a -> { block: Block.t } -> 'b; *)
  visit_block_statement: 'a -> Block.t -> 'b;
  visit_exit: 'a -> 'b;
  visit_empty_statement: 'a -> 'b;
  visit_return: 'a -> Expression.t option -> 'b;
  visit_switch: 'a -> Expression.t -> Statement.switch_case list -> 'b;
  visit_declaration_statement: 'a -> Declaration.t -> 'b;
}

(* TODO discards the associated StmType *)
let rec statement_visit_helper v acc (s_info: Prog.Statement.t) =
  let s_typ = snd s_info in
  match s_typ.stmt with
  | MethodCall {func; type_args; args} ->
    v.visit_method_call acc func type_args args
  | Assignment {lhs; rhs} -> v.visit_assignment acc lhs rhs
  | DirectApplication {typ; args} -> v.visit_direct_application acc typ args
  | Conditional {cond; tru; fls} ->
    let (acc_tru, acc_fls) = v.enter_conditional acc cond in
    begin match fls with
      | None -> v.exit_conditional
        (statement_visit_helper v acc_tru tru) None
      | Some fls_branch -> v.exit_conditional
        (statement_visit_helper v acc_tru tru)
        (Some (statement_visit_helper v acc_fls fls_branch))
    end
  | BlockStatement {block} -> v.visit_block_statement acc block
  | Exit -> v.visit_exit acc
  | EmptyStatement -> v.visit_empty_statement acc
  | Return {expr} -> v.visit_return acc expr
  | Switch {expr; cases} -> v.visit_switch acc expr cases
  | DeclarationStatement {decl} -> v.visit_declaration_statement acc decl

(*
Block not recursive
Blocks contain annotations and statements
Declarations and statements contain blocks
Annotations contain expressions and Key Values
*)
type ('a, 'b) block_visitor = {
  visit_block: 'a -> Annotation.t list -> Statement.t list -> 'b;
}

let block_visit_helper v acc (b_info: Prog.Block.t) =
  let b = snd b_info in
  v.visit_block acc b.annotations b.statements

type ('a, 'b) parser_visitor = {
  enter_state: 'a -> Annotation.t list -> P4String.t -> Statement.t list -> 'a;
  exit_state: 'b -> 'b;
  (* these are for transitions *)
  visit_direct: 'a -> P4String.t -> 'b;
  visit_select_nil: 'a -> Expression.t list -> 'b;
  enter_select_cons: 'a -> ('a * 'a);
  exit_select_cons: 'b -> 'b -> 'b;
  (* lastly, for cases *)
  visit_case: 'a -> Match.t list -> P4String.t -> 'b;
}

let parser_visit_helper v acc (p_info: Prog.Parser.state) =
  let p = snd p_info in
  let acc' = v.enter_state acc p.annotations p.name p.statements in
  let rec select_loop acc_loop expr_list (cases_list: Parser.case list) =
    match cases_list with
    | [] -> v.visit_select_nil acc_loop expr_list
    | (_, h) :: t -> let (acc_h, acc_t) = v.enter_select_cons acc_loop in
      v.exit_select_cons
        (v.visit_case acc_h h.matches h.next)
        (select_loop acc_t expr_list t)
  in
  match snd p.transition with
  | Direct {next} -> v.visit_direct acc' next
  | Select {exprs; cases} -> select_loop acc' exprs cases

type ('a, 'b) program_visitor = {
  visit_program_nil: 'a -> 'b;
  visit_single_declaration: 'a -> Declaration.t -> 'b;
  enter_program_cons: 'a -> ('a * 'a);
  exit_program_cons: 'b -> 'b -> 'b;
}

(**
  The Program type does not contain info.
*)
let rec program_visit_helper v acc (p: Prog.program) =
  match p with
  | Program [] -> v.visit_program_nil acc
  | Program (h :: t) ->
    let (acc_h, acc_t) = v.enter_program_cons acc in
    let out_h = v.visit_single_declaration acc_h h in
    let out_t = program_visit_helper v acc_t (Program t) in
    v.exit_program_cons out_h out_t

(* Example uses of the visitor structures *)

(**
  This visitor determines the depth of a Type.t.  It does not take the
  mutual recursion of Petr4 AST types into account:  it treats any
  Expression.t or P4String.t within the Type.t as having no depth.
 *)
(*
let type_depth_visitor =
  let base = (fun n -> n) in
  let base_ignore_term = (fun n _ -> n) in
  let incr = (fun n -> n + 1) in
  let incr_ignore_term = (fun n _ -> n + 1) in
  let split = (fun n -> (n + 1, n + 1)) in {
  visit_bool = base;
  visit_error = base;
  visit_integer = base;
  visit_int_type = base_ignore_term;
  visit_bit_type = base_ignore_term;
  visit_var_bit = base_ignore_term;
  visit_top_level_type = base_ignore_term;
  visit_type_name = base_ignore_term;
  enter_specialized_type_nil = incr;
  exit_specialized_type_nil = base;
  enter_specialized_type_cons = split;
  exit_specialized_type_cons = max;
  enter_header_stack = incr_ignore_term;
  exit_header_stack = base;
  visit_tuple_nil = base;
  enter_tuple_cons = split;
  exit_tuple_cons = max;
  visit_string = base;
  visit_void = base;
  visit_dont_care = base;
}

let type_depth =
  type_visit_helper type_depth_visitor 0
*)

(*
let type_depth_bottom_up_visitor =
  let base = (fun _ -> 0) in
  let base_ignore_term = (fun _ _ -> 0) in
  let up1 = (fun n -> 1 + n) in
  let down1 = (fun _ -> ()) in
  let down1_ignore_term = (fun _ _ -> ()) in
  let down2 = (fun _ -> ((), ())) in
  let up2 = (fun n m -> 1 + max n m) in {
  visit_bool = base;
  visit_error = base;
  visit_integer = base;
  visit_int_type = base_ignore_term;
  visit_bit_type = base_ignore_term;
  visit_var_bit = base_ignore_term;
  visit_top_level_type = base_ignore_term;
  visit_type_name = base_ignore_term;
  enter_specialized_type_nil = down1;
  exit_specialized_type_nil = up1;
  enter_specialized_type_cons = down2;
  exit_specialized_type_cons = up2;
  enter_header_stack = down1_ignore_term;
  exit_header_stack = up1;
  visit_tuple_nil = base;
  enter_tuple_cons = down2;
  exit_tuple_cons = up2;
  visit_string = base;
  visit_void = base;
  visit_dont_care = base;
}

let type_depth_bottom_up =
  type_visit_helper type_depth_bottom_up_visitor ()
*)

(**
  This visitor determines the number of nodes in a Statement.t.  It ignores
  all other Petr4 AST types.
*)
let statement_count_visitor =
  let base1 = (fun _ -> 1) in
  let base2 = (fun _ _ -> 1) in
  let base3 = (fun _ _ _ -> 1) in
  let base4 = (fun _ _ _ _ -> 1) in
  let split = (fun _ _ -> ((), ())) in {
  visit_method_call = base4;
  visit_assignment = base3;
  visit_direct_application = base3;
  enter_conditional = split;
  exit_conditional =
    begin fun n1 n2 ->
      match n2 with
      | None -> n1 + 1
      | Some n2' -> n1 + n2' + 1
    end;
  visit_block_statement = base2;
  visit_exit = base1;
  visit_empty_statement = base1;
  visit_return = base2;
  visit_switch = base3;
  visit_declaration_statement = base2;
}

let statement_count =
  statement_visit_helper statement_count_visitor ()

(* TODO Ignore may not actually have a use *)
type header_visit_state =
  | Search
  | Construct of string
  | Ignore

let end_construct x =
  match x with
  | Construct _ -> Search
  | _ -> x

let rec key_value_headers_visitor = {
  visit_key_value = begin fun acc _ e_info ->
    expression_visit_helper expression_headers_visitor acc e_info
  end
}

(**
  TODO This is just a guess at how header structure works now.
  Compose headers from ExpressionMember constructors whose children are either
  Name or ExpressionMember.
*)
and expression_headers_visitor =
  let base1 = (fun _ -> []) in
  let base2 = (fun _ _ -> []) in
  let base3 = (fun _ _ _ -> []) in
  (* let enter1 = (fun x -> x) in *)
  (* let enter1 = end_construct in *)
  (* let enter2 = (fun x -> (x, x)) in *)
  let enter2 = (fun x -> let x' = end_construct x in (x', x')) in
  (* let enter3 = (fun x -> (x, x, x)) in *)
  let enter3 = (fun x -> let x' = end_construct x in (x', x', x')) in
  let enter1_ignore1 = (fun x _ -> end_construct x) in
  let enter1_ignore2 = (fun x _ _ -> end_construct x) in
  let enter2_ignore1 = (fun x _ -> let x' = end_construct x in (x', x')) in
  let exit1 = (fun l -> l) in
  let exit2 = (@) in
  let exit3 = (fun l1 l2 l3 -> l1 @ (l2 @ l3)) in {
  visit_true = base1;
  visit_false = base1;
  visit_int = base2;
  visit_string = base2;
  visit_name = begin fun x s_info ->
    match x with
    | Search
    | Ignore -> []
    | Construct s -> [s ^ "." ^ (snd s_info)]
    end;
  visit_top_level = base2;
  enter_array_access = enter2;
  exit_array_access = exit2;
  enter_bit_string_access = enter1_ignore2;
  exit_bit_string_access = exit1;
  visit_list_nil = base1;
  enter_list_cons = enter2;
  exit_list_cons = exit2;
  visit_record = begin fun x kv_list ->
    let header_lists =
      List.map
        (key_value_visit_helper key_value_headers_visitor (end_construct x))
        kv_list
    in List.fold_left (@) [] header_lists
    end;
  (* (fun x -> key_value_visit_helper key_value_headers_visitor x); *)
  enter_unary_op = enter1_ignore1;
  exit_unary_op = exit1;
  enter_binary_op = enter2_ignore1;
  exit_binary_op = exit2;
  enter_cast = enter1_ignore1;
  exit_cast = exit1;
  visit_type_member = base3;
  visit_error_member = base2;
  enter_expression_member = begin fun x s_info ->
    match x with
    | Search -> Construct (snd s_info)
    | Construct s -> Construct (s ^ "." ^ (snd s_info))
    | Ignore -> Ignore
    end;
  exit_expression_member = exit1;
  (* begin fun lst ->
    match lst with
    | _ :: _ -> failwith "Only one header name should be found"
    | _ -> lst
    end; *)
  enter_ternary = enter3;
  exit_ternary = exit3;
  enter_function_call_nil = enter1_ignore1;
  exit_function_call_nil = exit1;
  enter_function_call_cons = enter2;
  exit_function_call_cons = begin fun l1 l2 ->
    match l1 with
    | Some l1' -> exit2 l1' l2
    | None -> exit1 l2
    end;
  visit_nameless_instantiation_nil = base2;
  enter_nameless_instantiation_cons = enter2;
  exit_nameless_instantiation_cons = exit2;
  visit_dont_care = base1;
  enter_mask = enter2;
  exit_mask = exit2;
  enter_range = enter2;
  exit_range = exit2;
}

let headers_in_expression =
  expression_visit_helper expression_headers_visitor Search

(**
  This is the start of a group of visitors for collecting all of the headers
  used in a program.
  Currently, this visitor ignores mutual recursion.
  Declarations, Statements, and Blocks are mutually recursive.
  A Parser can contain a Statement, and a Parser can be contained in a
  Declaration.
  A Statement can be contained in either a Block or a Parser.
  A Block can be contained in a Statement or a Declaration.
  A Declaration can be contained in a Program or a Statement.
  A Parser can be contained in a Declaration.
  A Program cannot be contained in anything.
*)
(* TODO my interpretation of "header uses" was wrong *)
let rec declaration_headers_visitor =
  let get_header_name = (fun _ _ name _ -> [name]) in
  let base2 = (fun _ _ -> []) in
  let base4 = (fun _ _ _ _ -> []) in
  let base5 = (fun _ _ _ _ _ -> []) in
  let base6 = (fun _ _ _ _ _ _ -> []) in
  let base9 = (fun _ _ _ _ _ _ _ _ _ -> []) in
  let enter1_ignore = (fun _ _ _ -> ()) in
  let enter2 = (fun _ -> ((), ())) in
  let exit1 = (fun l -> l) in
  let exit2 = (@) in
  let block_helper = (fun b ->
    block_visit_helper block_headers_visitor () b) in
  let block5 = (fun _ _ _ _ _ -> block_helper) in
  let block6 = (fun _ _ _ _ _ _ -> block_helper) in {
  visit_constant = base5;
  visit_instantiation = (fun _ _ _ _ _ block_opt ->
    match block_opt with
    | None -> []
    | Some b -> block_helper b
  );
  visit_parser_nil = (fun _ _ _ _ _ _ states ->
    let header_lists =
      List.map
        (parser_visit_helper parser_headers_visitor [])
        states
    in List.fold_left (@) [] header_lists
  );
  enter_parser_cons = enter2;
  exit_parser_cons = exit2;
  visit_control_nil = block6;
  enter_control_cons = enter2;
  exit_control_cons = exit2;
  visit_function = block5;
  visit_extern_function = base6;
  visit_variable = base5;
  visit_value_set = base5;
  visit_action = block5;
  visit_table = base9;
  visit_header = get_header_name;
  visit_header_union = get_header_name;
  visit_struct = base4;
  visit_error = base2;
  visit_match_kind = base2;
  visit_enum = base4;
  visit_serializable_enum = base5;
  visit_extern_object = base5;
  visit_type_def_type = base4;
  enter_type_def_decl = enter1_ignore;
  exit_type_def_decl = exit1;
  visit_new_type_type = base4;
  enter_new_type_decl = enter1_ignore;
  exit_new_type_decl = exit1;
  visit_control_type = base5;
  visit_parser_type = base5;
  visit_package_type = base5;
}

(* Statement can contain Declaration or Block, but not Parser *)
and statement_headers_visitor =
  let base1 = (fun _ -> []) in
  let base2 = (fun _ _ -> []) in
  let base3 = (fun _ _ _ -> []) in
  let base4 = (fun _ _ _ _ -> []) in
  let split = (fun _ _ -> ((), ())) in {
  visit_method_call = base4;
  visit_assignment = base3;
  visit_direct_application = base3;
  enter_conditional = split;
  exit_conditional =
    begin fun l1 l2 ->
      match l2 with
      | None -> l1
      | Some l2' -> l1 @ l2'
    end;
  visit_block_statement =
    (fun _ -> block_visit_helper block_headers_visitor ());
  visit_exit = base1;
  visit_empty_statement = base1;
  visit_return = base2;
  visit_switch = base3;
  visit_declaration_statement = (fun acc decl ->
    declaration_visit_helper declaration_headers_visitor acc decl);
}

(* TODO map not tail recursive *)
and block_headers_visitor = {
  visit_block = (fun acc _ statements ->
    let header_lists =
      List.map
        (statement_visit_helper statement_headers_visitor acc)
        statements
    in List.fold_left (@) [] header_lists
  );
}

(* TODO no use for Expressions *)
(* TODO make sure this is correct *)
and parser_headers_visitor =
  let base2 = (fun l _ -> l) in
  let base3 = (fun l _ _ -> l) in
  let enter2 = (fun l -> (l, l)) in
  let exit1 = (fun l -> l) in
  let exit2 = (@) in {
  enter_state = (fun _ _ _ statements ->
    let header_lists =
      List.map
        (statement_visit_helper statement_headers_visitor ())
        statements
    in List.fold_left (@) [] header_lists
  );
  exit_state = exit1;
  visit_direct = base2;
  visit_select_nil = base2;
  enter_select_cons = enter2;
  exit_select_cons = exit2;
  visit_case = base3;
}

let program_headers_visitor = {
  visit_program_nil= (fun _ -> []);
  visit_single_declaration =
    declaration_visit_helper declaration_headers_visitor;
  enter_program_cons = (fun _ -> ((), ()));
  exit_program_cons = (@);
}

let all_headers (p: Prog.program) =
  let headers = program_visit_helper program_headers_visitor () p in
  List.sort_uniq (Stdlib.compare) headers

end