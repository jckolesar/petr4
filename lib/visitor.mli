open Types
open Typed
open Prog

type ('a, 'b) type_parameter_visitor = {
  visit_type_parameter:
    'a -> Annotation.t list -> direction ->
    Type.t -> P4String.t -> Expression.t option -> 'b;
}

type ('a, 'b) method_prototype_visitor = {
  visit_constructor:
    'a -> Annotation.t list -> P4String.t ->
    TypeParameter.t list -> 'b;
  visit_abstract_method:
    'a -> Annotation.t list -> Type.t ->
    P4String.t -> P4String.t list ->
    TypeParameter.t list -> 'b;
  visit_method:
    'a -> Annotation.t list -> Type.t ->
    P4String.t -> P4String.t list ->
    TypeParameter.t list -> 'b;
}

type ('a, 'b) key_value_visitor = {
  visit_key_value: 'a -> P4String.t -> Expression.t -> 'b;
}

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

type ('a, 'b) match_visitor = {
  visit_dont_care: 'a -> 'b;
  visit_expression: 'a -> Expression.t -> 'b;
}

type ('a, 'b) declaration_visitor = {
  visit_constant:
    'a -> Annotation.t list -> Type.t -> P4String.t -> Value.value -> 'b;
  visit_instantiation:
    'a -> Annotation.t list -> Type.t -> Expression.t list ->
    P4String.t -> Block.t option -> 'b;
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

type ('a, 'b) statement_visitor = {
  visit_method_call:
    'a -> Expression.t -> Type.t list -> (Expression.t option) list -> 'b;
  visit_assignment: 'a -> Expression.t -> Expression.t -> 'b;
  visit_direct_application: 'a -> Type.t -> Expression.t list -> 'b;
  enter_conditional: 'a -> Expression.t -> ('a * 'a);
  exit_conditional: 'b -> 'b option -> 'b;
  visit_block_statement: 'a -> Block.t -> 'b;
  visit_exit: 'a -> 'b;
  visit_empty_statement: 'a -> 'b;
  visit_return: 'a -> Expression.t option -> 'b;
  visit_switch: 'a -> Expression.t -> Statement.switch_case list -> 'b;
  visit_declaration_statement: 'a -> Declaration.t -> 'b;
}

type ('a, 'b) block_visitor = {
  visit_block: 'a -> Annotation.t list -> Statement.t list -> 'b;
}

type ('a, 'b) parser_visitor = {
  enter_state: 'a -> Annotation.t list -> P4String.t -> Statement.t list -> 'a;
  exit_state: 'b -> 'b;
  visit_direct: 'a -> P4String.t -> 'b;
  visit_select_nil: 'a -> Expression.t list -> 'b;
  enter_select_cons: 'a -> ('a * 'a);
  exit_select_cons: 'b -> 'b -> 'b;
  visit_case: 'a -> Match.t list -> P4String.t -> 'b;
}

type ('a, 'b) program_visitor = {
  visit_program_nil: 'a -> 'b;
  visit_single_declaration: 'a -> Declaration.t -> 'b;
  enter_program_cons: 'a -> ('a * 'a);
  exit_program_cons: 'b -> 'b -> 'b;
}

val type_parameter_visit_helper:
  ('a, 'b) type_parameter_visitor -> 'a -> Prog.TypeParameter.t -> 'b

val method_prototype_visit_helper:
  ('a, 'b) method_prototype_visitor -> 'a -> Prog.MethodPrototype.t -> 'b

val key_value_visit_helper:
  ('a, 'b) key_value_visitor -> 'a -> Prog.KeyValue.t -> 'b

val expression_visit_helper:
  ('a, 'b) expression_visitor -> 'a -> Prog.Expression.t -> 'b

val match_visit_helper:
  ('a, 'b) match_visitor -> 'a -> Prog.Match.t -> 'b

val declaration_visit_helper:
  ('a, 'b) declaration_visitor -> 'a -> Prog.Declaration.t -> 'b

val statement_visit_helper:
  ('a, 'b) statement_visitor -> 'a -> Prog.Statement.t -> 'b

val block_visit_helper:
  ('a, 'b) block_visitor -> 'a -> Prog.Block.t -> 'b

val parser_visit_helper:
  ('a, 'b) parser_visitor -> 'a -> Prog.Parser.state -> 'b

val program_visit_helper:
  ('a, 'b) program_visitor -> 'a -> Prog.program -> 'b

val headers_in_expression: Prog.Expression.t -> string list

val get_all_headers: Prog.program -> string list