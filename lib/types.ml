(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
*)

open Util

open Sexplib.Conv

type 'a info = Info.t * 'a [@@deriving sexp,yojson]

let info (i,_) = i

let info_to_yojson f (_,x) = f x

let info_of_yojson f json =
  match f json with
  | Ok pre -> Ok (Info.M "<yojson>", pre)
  | Error x -> Error x

module P4Int = struct

  type pre_t =
    { value: bigint;
      width_signed: (int * bool) option }
  [@@deriving sexp,yojson]

  type t = pre_t info [@@deriving sexp,yojson]
end

module P4String = struct
  type t = string info
  [@@deriving sexp,yojson]
end

module rec KeyValue : sig
  type pre_t = 
    { key : P4String.t;
      value : Expression.t } 
  [@@deriving sexp,yojson]

  type t = pre_t info [@@deriving sexp,yojson]
end = struct
  type pre_t = 
    { key : P4String.t;
      value : Expression.t } 
  [@@deriving sexp,yojson]

  type t = pre_t info [@@deriving sexp,yojson]
end

and Annotation : sig
  type pre_body = 
    | Empty
    | Unparsed of P4String.t list
    | Expression of Expression.t list
    | KeyValue of KeyValue.t list
  [@@deriving sexp,yojson]

  type body = pre_body info [@@deriving sexp,yojson]

  type pre_t =
    { name: P4String.t;
      body: body }
  [@@deriving sexp,yojson]

  type t = pre_t info [@@deriving sexp,yojson]
end = struct
  type pre_body = 
    | Empty
    | Unparsed of P4String.t list
    | Expression of Expression.t list
    | KeyValue of KeyValue.t list
  [@@deriving sexp,yojson]

  type body = pre_body info [@@deriving sexp,yojson]

  type pre_t =
    { name: P4String.t;
      body: body }
  [@@deriving sexp,yojson]

  type t = pre_t info [@@deriving sexp,yojson]
end

and Parameter : sig
  type pre_t =
    { annotations: Annotation.t list;
      direction: Direction.t option;
      typ: Type.t;
      variable: P4String.t;
      opt_value: Expression.t option}
  [@@deriving sexp,yojson]

  type t = pre_t info [@@deriving sexp,yojson]
end = struct
  type pre_t =
    { annotations: Annotation.t list;
      direction: Direction.t option;
      typ: Type.t [@name "type"];
      variable: P4String.t;
      opt_value: Expression.t option}
  [@@deriving sexp,yojson]

  type t = pre_t info [@@deriving sexp,yojson]
end

and Op : sig
  type pre_uni =
      Not
    | BitNot
    | UMinus
  [@@deriving sexp,yojson]

  type uni = pre_uni info [@@deriving sexp,yojson]

  type pre_bin =
      Plus
    | PlusSat
    | Minus
    | MinusSat
    | Mul
    | Div
    | Mod
    | Shl
    | Shr
    | Le
    | Ge
    | Lt
    | Gt
    | Eq
    | NotEq
    | BitAnd
    | BitXor
    | BitOr
    | PlusPlus
    | And
    | Or
  [@@deriving sexp,yojson]

  type bin = pre_bin info [@@deriving sexp,yojson]
end = struct
  type pre_uni =
      Not
    | BitNot
    | UMinus
  [@@deriving sexp,yojson]

  type uni = pre_uni info [@@deriving sexp,yojson]

  type pre_bin =
      Plus
    | PlusSat
    | Minus
    | MinusSat
    | Mul
    | Div
    | Mod
    | Shl
    | Shr
    | Le
    | Ge
    | Lt
    | Gt
    | Eq
    | NotEq
    | BitAnd
    | BitXor
    | BitOr
    | PlusPlus
    | And
    | Or
  [@@deriving sexp,yojson]

  type bin = pre_bin info [@@deriving sexp,yojson]
end

and Type : sig
  type pre_t =
      Bool
    | Error
    | Integer
    | IntType of Expression.t
    | BitType of Expression.t
    | VarBit of Expression.t
    | TopLevelType of P4String.t
    (* this could be a typename or a type variable. *)
    | TypeName of P4String.t
    | SpecializedType of
        { base: t;
          args: t list }
    | HeaderStack of
        { header: t;
          size:  Expression.t }
    | Tuple of t list
    | String
    | Void
    | DontCare
  [@@deriving sexp,yojson]

and t = pre_t info [@@deriving sexp,yojson]
end = struct
  type pre_t =
      Bool [@name "bool"]
    | Error [@name "error"]
    | Integer [@name "integer"]
    | IntType of Expression.t [@name "int"]
    | BitType of Expression.t  [@name "bit"]
    | VarBit of Expression.t  [@name "varbit"]
    | TopLevelType of P4String.t [@name "top_level"]
    | TypeName of P4String.t [@name "name"]
    | SpecializedType of
        { base: t;
          args: t list } [@name "specialized"]
    | HeaderStack of
        { header: t;
          size:  Expression.t } [@name "header_stack"]
    | Tuple of t list [@name "tuple"]
    | String [@name "string"]
    | Void [@name "void"]
    | DontCare [@name "dont_care"]
  [@@deriving sexp,yojson]

and t = pre_t info [@@deriving sexp,yojson]
end

and MethodPrototype : sig
  type pre_t =
    Constructor of
      { annotations: Annotation.t list;
        name: P4String.t;
        params: Parameter.t list }
  | AbstractMethod of
      { annotations: Annotation.t list;
        return: Type.t;
        name: P4String.t;
        type_params: P4String.t list;
        params: Parameter.t list}
  | Method of
      { annotations: Annotation.t list;
        return: Type.t;
        name: P4String.t;
        type_params: P4String.t list;
        params: Parameter.t list}
        [@@deriving sexp,yojson]
    
  type t = pre_t info [@@deriving sexp,yojson]
end = struct
  type pre_t =
    Constructor of
      { annotations: Annotation.t list;
        name: P4String.t;
        params: Parameter.t list }
  | AbstractMethod of
      { annotations: Annotation.t list;
        return: Type.t;
        name: P4String.t;
        type_params: P4String.t list;
        params: Parameter.t list}
  | Method of
      { annotations: Annotation.t list;
        return: Type.t;
        name: P4String.t;
        type_params: P4String.t list;
        params: Parameter.t list}
    [@@deriving sexp,yojson]
    
  type t = pre_t info [@@deriving sexp,yojson]
end
        
and Argument : sig
      type pre_t  =
          Expression of
            { value: Expression.t }
        | KeyValue of
            { key: P4String.t;
              value: Expression.t }
        | Missing
      [@@deriving sexp,yojson]

      type t = pre_t info [@@deriving sexp,yojson]
    end = struct
                 type pre_t  =
                     Expression of
                       { value: Expression.t }
                   | KeyValue of
                       { key: P4String.t;
                         value: Expression.t }
                   | Missing
                 [@@deriving sexp,yojson]

                 type t = pre_t info [@@deriving sexp,yojson]
               end

and Direction : sig
      type pre_t =
          In
        | Out
        | InOut
      [@@deriving sexp,yojson]

      type t = pre_t info [@@deriving sexp,yojson]
    end = struct
                  type pre_t =
                      In
                    | Out
                    | InOut
                  [@@deriving sexp,yojson]

                  type t = pre_t info [@@deriving sexp,yojson]
                end

and Expression : sig
      type pre_t =
          True
        | False
        | Int of P4Int.t
        | String of P4String.t
        | Name of P4String.t
        | TopLevel of P4String.t
        | ArrayAccess of
            { array: t;
              index: t }
        | BitStringAccess of
            { bits: t;
              lo: t;
              hi: t }
        | List of
            { values: t list }
        | Record of
            { entries: KeyValue.t list }
        | UnaryOp of
            { op: Op.uni;
              arg: t }
        | BinaryOp of
            { op: Op.bin;
              args: (t * t) }
        | Cast of
            { typ: Type.t;
              expr: t }
        | TypeMember of
            { typ: Type.t;
              name: P4String.t }
        | ErrorMember of P4String.t
        | ExpressionMember of
            { expr: t;
              name: P4String.t }
        | Ternary of
            { cond: t;
              tru: t;
              fls: t }
        | FunctionCall of
            { func: t;
              type_args: Type.t list;
              args: Argument.t list }
        | NamelessInstantiation of
            { typ: Type.t [@key "type"];
              args: Argument.t list }
        | Mask of
            { expr: t;
              mask: t }
        | Range of
            { lo: t;
              hi: t }
      [@@deriving sexp,yojson]

and t = pre_t info [@@deriving sexp,yojson]
end = struct
  type pre_t =
      True [@name "true"]
    | False  [@name "false"]
    | Int of P4Int.t  [@name "int"]
    | String of P4String.t [@name "string"]
    | Name of P4String.t [@name "name"]
    | TopLevel of P4String.t [@name "top_level"]
    | ArrayAccess of
        { array: t;
          index: t } [@name "array_access"]
    | BitStringAccess of
        { bits: t;
          lo: t;
          hi: t } [@name "bit_string_access"]
    | List of
        { values: t list } [@name "list"]
    | Record of
        { entries: KeyValue.t list } [@name "struct"]
    | UnaryOp of
        { op: Op.uni;
          arg: t } [@name "unary_op"]
    | BinaryOp of
        { op: Op.bin;
          args: (t * t) } [@name "binary_op"]
    | Cast of
        { typ: Type.t [@key "type"];
          expr: t }  [@name "cast"]
    | TypeMember of
        { typ: Type.t [@key "type"];
          name: P4String.t } [@name "type_member"]
    | ErrorMember of P4String.t [@name "error_member"]
    | ExpressionMember of
        { expr: t;
          name: P4String.t } [@name "expression_member"]
    | Ternary of
        { cond: t;
          tru: t;
          fls: t } [@name "ternary"]
    | FunctionCall of
        { func: t;
          type_args: Type.t list;
          args: Argument.t list } [@name "call"]
    | NamelessInstantiation of
        { typ: Type.t [@key "type"];
          args: Argument.t list } [@name "instantiation"]
    | Mask of
        { expr: t;
          mask: t } [@name "mask"]
    | Range of
        { lo: t;
          hi: t } [@name "range"]
  [@@deriving sexp,yojson]

and t = pre_t info [@@deriving sexp,yojson]
end

and Table : sig
      type pre_action_ref =
        { annotations: Annotation.t list;
          name: P4String.t;
          args: Argument.t list }
      [@@deriving sexp,yojson]

      type action_ref = pre_action_ref info [@@deriving sexp,yojson]

      type pre_key =
        { annotations: Annotation.t list;
          key: Expression.t;
          match_kind: P4String.t }
      [@@deriving sexp,yojson]

      type key = pre_key info [@@deriving sexp,yojson]

      type pre_entry =
        { annotations: Annotation.t list;
          matches: Match.t list;
          action: action_ref }
      [@@deriving sexp,yojson { exn = true }]

      type entry = pre_entry info [@@deriving sexp,yojson]

      type pre_property =
          Key of
            { keys: key list }
        | Actions of
            { actions: action_ref list }
        | Entries of
            { entries: entry list }
        | Custom of
            { annotations: Annotation.t list;
              const: bool;
              name: P4String.t;
              value: Expression.t }
      [@@deriving sexp,yojson]

      type property = pre_property info [@@deriving sexp,yojson]

      val name_of_property : property -> string
    end = struct
              type pre_action_ref =
                { annotations: Annotation.t list;
                  name: P4String.t;
                  args: Argument.t list }
              [@@deriving sexp,yojson]

              type action_ref = pre_action_ref info [@@deriving sexp,yojson]

              type pre_key =
                { annotations: Annotation.t list;
                  key: Expression.t;
                  match_kind: P4String.t }
              [@@deriving sexp,yojson]

              type key = pre_key info [@@deriving sexp,yojson]

              type pre_entry =
                { annotations: Annotation.t list;
                  matches: Match.t list;
                  action: action_ref }
              [@@deriving sexp,yojson { exn = true }]

              type entry = pre_entry info [@@deriving sexp,yojson]

              type pre_property =
                  Key of
                    { keys: key list }
                | Actions of
                    { actions: action_ref list }
                | Entries of
                    { entries: entry list }
                | Custom of
                    { annotations: Annotation.t list;
                      const: bool;
                      name: P4String.t;
                      value: Expression.t }
              [@@deriving sexp,yojson]

              type property = pre_property info [@@deriving sexp,yojson]

              let name_of_property p =
                match snd p with
                | Key _ -> "key"
                | Actions _ -> "actions"
                | Entries _ -> "entries"
                | Custom {name; _} -> snd name
            end

and Match : sig
      type pre_t =
          Default
        | DontCare
        | Expression of
            { expr: Expression.t }
      [@@deriving sexp,yojson { exn = true }]

      type t = pre_t info [@@deriving sexp,yojson { exn = true }]
    end = struct
              type pre_t =
                  Default
                | DontCare
                | Expression of
                    { expr: Expression.t }
              [@@deriving sexp,yojson { exn = true }]

              type t = pre_t info [@@deriving sexp,yojson { exn = true }]
            end

and Parser : sig
      type pre_case =
        { matches: Match.t list;
          next: P4String.t }
      [@@deriving sexp,yojson { exn = true }]

      type case = pre_case info [@@deriving sexp,yojson]

      type pre_transition =
          Direct of
            { next: P4String.t }
        | Select of
            { exprs: Expression.t list;
              cases: case list }
      [@@deriving sexp,yojson]

      type transition = pre_transition info [@@deriving sexp,yojson]

      type pre_state =
        { annotations: Annotation.t list;
          name: P4String.t;
          statements: Statement.t list;
          transition: transition }
      [@@deriving sexp,yojson]

      type state = pre_state info [@@deriving sexp,yojson]
    end = struct
               type pre_case =
                 { matches: Match.t list;
                   next: P4String.t }
               [@@deriving sexp,yojson { exn = true }]

               type case = pre_case info [@@deriving sexp,yojson]

               type pre_transition =
                   Direct of
                     { next: P4String.t }
                 | Select of
                     { exprs: Expression.t list;
                       cases: case list }
               [@@deriving sexp,yojson]

               type transition = pre_transition info [@@deriving sexp,yojson]

               type pre_state =
                 { annotations: Annotation.t list;
                   name: P4String.t;
                   statements: Statement.t list;
                   transition: transition }
               [@@deriving sexp,yojson]

               type state = pre_state info [@@deriving sexp,yojson]
             end

and Declaration : sig
      type pre_t =
          Constant of
            { annotations: Annotation.t list;
              typ: Type.t [@key "type"];
              name: P4String.t;
              value: Expression.t }
        | Instantiation of
            { annotations: Annotation.t list;
              typ: Type.t [@key "type"];
              args: Argument.t list;
              name: P4String.t;
              init: Block.t option; }
        | Parser of
            { annotations: Annotation.t list;
              name: P4String.t;
              type_params: P4String.t list;
              params: Parameter.t list;
              constructor_params: Parameter.t list;
              locals: t list;
              states: Parser.state list }
        | Control of
            { annotations: Annotation.t list;
              name: P4String.t;
              type_params: P4String.t list;
              params: Parameter.t list;
              constructor_params: Parameter.t list;
              locals: t list;
              apply: Block.t }
        | Function of
            { return: Type.t;
              name: P4String.t;
              type_params: P4String.t list;
              params: Parameter.t list;
              body: Block.t }
        | ExternFunction of
            { annotations: Annotation.t list;
              return: Type.t;
              name: P4String.t;
              type_params: P4String.t list;
              params: Parameter.t list }
        | Variable of
            { annotations: Annotation.t list;
              typ: Type.t [@key "type"];
              name: P4String.t;
              init: Expression.t option }
        | ValueSet of
            { annotations: Annotation.t list;
              typ: Type.t [@key "type"];
              size: Expression.t;
              name: P4String.t }
        | Action of
            { annotations: Annotation.t list;
              name: P4String.t;
              params: Parameter.t list;
              body: Block.t }
        | Table of
            { annotations: Annotation.t list;
              name: P4String.t;
              properties: Table.property list }
        | Header of
            { annotations: Annotation.t list;
              name: P4String.t;
              fields: field list }
        | HeaderUnion of
            { annotations: Annotation.t list;
              name: P4String.t;
              fields: field list }
        | Struct of
            { annotations: Annotation.t list;
              name: P4String.t;
              fields: field list }
        | Error of
            { members: P4String.t list }
        | MatchKind of
            { members: P4String.t list }
        | Enum of
            { annotations: Annotation.t list;
              name: P4String.t;
              members: P4String.t list }
        | SerializableEnum of
            { annotations: Annotation.t list;
              typ: Type.t [@key "type"];
              name: P4String.t;
              members: (P4String.t * Expression.t) list }
        | ExternObject of
            { annotations: Annotation.t list;
              name: P4String.t;
              type_params: P4String.t list;
              methods: MethodPrototype.t list }
        | TypeDef of
            { annotations: Annotation.t list;
              name: P4String.t;
              typ_or_decl: (Type.t, t) alternative }
        | NewType of
            { annotations: Annotation.t list;
              name: P4String.t;
              typ_or_decl: (Type.t, t) alternative }
        | ControlType of
            { annotations: Annotation.t list;
              name: P4String.t;
              type_params: P4String.t list;
              params: Parameter.t list }
        | ParserType of
            { annotations: Annotation.t list;
              name: P4String.t;
              type_params: P4String.t list;
              params: Parameter.t list }
        | PackageType of
            { annotations: Annotation.t list;
              name: P4String.t;
              type_params: P4String.t list;
              params: Parameter.t list }
      [@@deriving sexp,yojson]

and t = pre_t info [@@deriving sexp,yojson]

and pre_field =
    { annotations: Annotation.t list;
      typ: Type.t [@key "type"];
      name: P4String.t } [@@deriving sexp,yojson]

and field = pre_field info [@@deriving sexp,yojson]

val name : t -> P4String.t

val name_opt : t -> P4String.t option

end = struct
  type pre_t =
      Constant of
        { annotations: Annotation.t list;
          typ: Type.t [@key "type"];
          name: P4String.t;
          value: Expression.t }
    | Instantiation of
        { annotations: Annotation.t list;
          typ: Type.t [@key "type"];
          args: Argument.t list;
          name: P4String.t;
          init: Block.t option; }
    | Parser of
        { annotations: Annotation.t list;
          name: P4String.t;
          type_params: P4String.t list;
          params: Parameter.t list;
          constructor_params: Parameter.t list;
          locals: t list;
          states: Parser.state list }
    | Control of
        { annotations: Annotation.t list;
          name: P4String.t;
          type_params: P4String.t list;
          params: Parameter.t list;
          constructor_params: Parameter.t list;
          locals: t list;
          apply: Block.t }
          [@name "control"]
    | Function of
        { return: Type.t;
          name: P4String.t;
          type_params: P4String.t list;
          params: Parameter.t list;
          body: Block.t }
    | ExternFunction of
        { annotations: Annotation.t list;
          return: Type.t;
          name: P4String.t;
          type_params: P4String.t list;
          params: Parameter.t list }
    | Variable of
        { annotations: Annotation.t list;
          typ: Type.t [@key "type"];
          name: P4String.t;
          init: Expression.t option }
    | ValueSet of
        { annotations: Annotation.t list;
          typ: Type.t [@key "type"];
          size: Expression.t;
          name: P4String.t }
    | Action of
        { annotations: Annotation.t list;
          name: P4String.t;
          params: Parameter.t list;
          body: Block.t }
    | Table of
        { annotations: Annotation.t list;
          name: P4String.t;
          properties: Table.property list }
    | Header of
        { annotations: Annotation.t list;
          name: P4String.t;
          fields: field list }
    | HeaderUnion of
        { annotations: Annotation.t list;
          name: P4String.t;
          fields: field list }
    | Struct of
        { annotations: Annotation.t list;
          name: P4String.t;
          fields: field list }
    | Error of
        { members: P4String.t list }
    | MatchKind of
        { members: P4String.t list }
    | Enum of
        { annotations: Annotation.t list;
          name: P4String.t;
          members: P4String.t list }
    | SerializableEnum of
        { annotations: Annotation.t list;
          typ: Type.t [@key "type"];
          name: P4String.t;
          members: (P4String.t * Expression.t) list }
    | ExternObject of
        { annotations: Annotation.t list;
          name: P4String.t;
          type_params: P4String.t list;
          methods: MethodPrototype.t list }
    | TypeDef of
        { annotations: Annotation.t list;
          name: P4String.t;
          typ_or_decl: (Type.t, t) alternative }
    | NewType of
        { annotations: Annotation.t list;
          name: P4String.t;
          typ_or_decl: (Type.t, t) alternative }
    | ControlType of
        { annotations: Annotation.t list;
          name: P4String.t;
          type_params: P4String.t list;
          params: Parameter.t list }
    | ParserType of
        { annotations: Annotation.t list;
          name: P4String.t;
          type_params: P4String.t list;
          params: Parameter.t list }
    | PackageType of
        { annotations: Annotation.t list;
          name: P4String.t;
          type_params: P4String.t list;
          params: Parameter.t list }
  [@@deriving sexp,yojson]

and t = pre_t info [@@deriving sexp,yojson]

and pre_field =
    { annotations: Annotation.t list;
      typ: Type.t [@key "type"];
      name: P4String.t } [@@deriving sexp,yojson]

and field = pre_field info [@@deriving sexp,yojson]

let name_opt d =
  match snd d with
  | Constant {name; _}
  | Instantiation {name; _}
  | Parser {name; _}
  | Control {name; _}
  | Function {name; _}
  | ExternFunction {name; _}
  | Variable {name; _}
  | ValueSet {name; _}
  | Action {name; _}
  | Table {name; _}
  | Header {name; _}
  | HeaderUnion {name; _}
  | Struct {name; _}
  | Enum {name; _}
  | SerializableEnum {name; _}
  | ExternObject {name; _}
  | TypeDef {name; _}
  | NewType {name; _}
  | ControlType {name; _}
  | ParserType {name; _}
  | PackageType {name; _} ->
      Some name
  | Error _
  | MatchKind _ ->
      None

let name d =
  match name_opt d with
  | Some name -> name
  | None -> failwith "this declaration does not have a name"
end

and Statement : sig
      type pre_switch_label =
          Default
        | Name of P4String.t
      [@@deriving sexp,yojson]

      type switch_label = pre_switch_label info [@@deriving sexp,yojson]

      type pre_switch_case =
          Action of
            { label: switch_label;
              code: Block.t }
        | FallThrough of
            { label: switch_label }
      [@@deriving sexp,yojson]

      type switch_case = pre_switch_case info [@@deriving sexp,yojson]

      type pre_t =
          MethodCall of
            { func: Expression.t;
              type_args: Type.t list;
              args: Argument.t list }
        | Assignment of
            { lhs: Expression.t;
              rhs: Expression.t }
        | DirectApplication of
            { typ: Type.t [@key "type"];
              args: Argument.t list }
        | Conditional of
            { cond: Expression.t;
              tru: t;
              fls: t option }
        | BlockStatement of
            { block: Block.t }
        | Exit
        | EmptyStatement
        | Return of
            { expr: Expression.t option }
        | Switch of
            { expr: Expression.t;
              cases: switch_case list }
        | DeclarationStatement of
            { decl: Declaration.t }
      [@@deriving sexp,yojson]

and t = pre_t info [@@deriving sexp,yojson]
end = struct
  type pre_switch_label =
      Default [@name "default"]
    | Name of P4String.t [@name "name"]
  [@@deriving sexp,yojson]

  type switch_label = pre_switch_label info [@@deriving sexp,yojson]

  type pre_switch_case =
      Action of
        { label: switch_label;
          code: Block.t }
    | FallThrough of
        { label: switch_label }
  [@@deriving sexp,yojson]

  type switch_case = pre_switch_case info [@@deriving sexp,yojson]

  type pre_t =
      MethodCall of
        { func: Expression.t;
          type_args: Type.t list;
          args: Argument.t list } [@name "method_call"]
    | Assignment of
        { lhs: Expression.t;
          rhs: Expression.t } [@name "assignment"]
    | DirectApplication of
        { typ: Type.t [@key "type"];
          args: Argument.t list } [@name "direct_application"]
    | Conditional of
        { cond: Expression.t;
          tru: t;
          fls: t option } [@name "conditional"]
    | BlockStatement of
        { block: Block.t } [@name "block"]
    | Exit [@name "exit"]
    | EmptyStatement [@name "empty_statement"]
    | Return of
        { expr: Expression.t option } [@name "return"]
    | Switch of
        { expr: Expression.t;
          cases: switch_case list } [@name "switch"]
    | DeclarationStatement of
        { decl: Declaration.t } [@name "declaration"]
  [@@deriving sexp,yojson]

and t = pre_t info [@@deriving sexp,yojson]
end

and Block : sig
      type pre_t =
        { annotations: Annotation.t list;
          statements: Statement.t list }
      [@@deriving sexp,yojson]

      type t = pre_t info [@@deriving sexp,yojson]
    end = struct
              type pre_t =
                { annotations: Annotation.t list;
                  statements: Statement.t list }
              [@@deriving sexp,yojson]

              type t = pre_t info [@@deriving sexp,yojson]
            end

type program =
    Program of Declaration.t list [@name "program"]
[@@deriving sexp,yojson]

(* TODO attempt at making a new tree visitor

A program is a list of declarations
*)

(*
TODO Annotation
They can contain arguments, which can contain expressions
However, they are not directly recursive
*)

(*
TODO Parameter
*)

type ('a, 'b) parameter_visitor = {
  visit_parameter:
    'a ->
    { annotations: Annotation.t list;
      direction: Direction.t option;
      typ: Type.t;
      variable: P4String.t;
      opt_value: Expression.t option} -> 'b;
}

let parameter_visit_helper v =
  v.visit_parameter

(*
TODO Op looks simple enough not to need anything special
*)

(*
TODO Type is recursive
These can contain expressions
*)

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

let rec type_visit_helper v acc t_info =
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
      v.exit_specialized_type_cons (type_visit_helper v acc_h h)
                                   (type_visit_helper v acc_t
                                   (SpecializedType {base = base; args = t}))
    end
  | HeaderStack {header; size} ->
    let acc' = v.enter_header_stack acc size in
    v.exit_header_stack (type_visit_helper v acc' header)
  | Tuple l -> begin match l with
    | [] -> v.visit_tuple_nil acc
    | h :: t -> let (acc_h, acc_t) = v.enter_tuple_cons acc in
      v.exit_tuple_cons (type_visit_helper v acc_h h)
                        (type_visit_helper acc_t (Tuple t))
    end
  | String -> v.visit_string acc
  | Void -> v.visit_void acc
  | DontCare -> v.visit_dont_care acc

(*
TODO MethodPrototype
They are not directly recursive; they can appear in declarations
Statements can contain declarations
Block and Parser can contain statements
Statements and declarations can contain blocks
Declarations can contain parsers
This means that there's no cycle back to MethodPrototype
*)

(*
TODO Argument
Expressions can contain arguments
Arguments can contain expressions, so there's a cycle
*)

type ('a, 'b) argument_visitor = {
  visit_expression: 'a -> { value: Expression.t } -> 'b;
  visit_key_value: 'a -> { value: Expression.t } -> 'b;
  visit_missing: 'a -> 'b;
}

(*
TODO Direction looks simple
No containment of other types at all
*)

type ('a, 'b) expression_visitor = {
  visit_true: 'a -> 'b;
  visit_false: 'a -> 'b;
  visit_int: 'a -> P4Int.t -> 'b;
  visit_string: 'a -> P4String.t -> 'b;
  visit_name: 'a -> P4String.t -> 'b;
  visit_top_level: 'a -> P4String.t -> 'b;
  enter_array_access: 'a -> ('a * 'a);
  exit_array_access: 'b -> 'b -> 'b;
  enter_bit_string_access: 'a -> ('a * 'a * 'a);
  exit_bit_string_access: 'b -> 'b -> 'b -> 'b;
  visit_list_nil: 'a -> 'b;
  (* head input/output, then tail input/output *)
  enter_list_cons: 'a -> ('a * 'a);
  exit_list_cons: 'b -> 'b -> 'b;
  visit_record: 'a -> { entries: KeyValue.t list } -> 'b;
  (* TODO is this input necessary for both enter and exit? *)
  enter_unary_op: 'a -> Op.uni -> 'a;
  exit_unary_op: 'b -> 'b;
  enter_binary_op: 'a -> Op.bin -> ('a * 'a);
  exit_binary_op: 'b -> 'b -> 'b;
  enter_cast: 'a -> Type.t -> 'a;
  exit_cast: 'b -> 'b;
  visit_type_member: 'a -> { typ: Type.t; name: P4String.t } -> 'b;
  visit_error_member: 'a -> P4String.t -> 'b;
  enter_expression_member: 'a -> P4String.t -> 'a;
  exit_expression_member: 'b -> 'b;
  enter_ternary: 'a -> ('a * 'a * 'a);
  exit_ternary: 'b -> 'b -> 'b -> 'b;
  (* TODO not sure how to have a regular way of handling cases like this *)
  (* reworking this to use the side information for now *)
  enter_function_call: 'a -> Type.t list -> Argument.t list -> 'a;
  exit_function_call: 'b -> 'b;
  visit_nameless_instantiation:
    'a ->
    { typ: Type.t;
      args: Argument.t list } -> 'b;
  enter_mask: 'a -> ('a * 'a);
  exit_mask: 'b -> 'b -> 'b;
  enter_range: 'a -> ('a * 'a);
  exit_range: 'b -> 'b -> 'b;
}

let rec expression_visit_helper v acc e_info =
  match snd e_info with
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
    let (acc_bits, acc_lo, acc_hi) = v.enter_bit_string_access acc in
    v.exit_bit_string_access (expression_visit_helper v acc_bits bits)
                             (expression_visit_helper v acc_lo lo)
                             (expression_visit_helper v acc_hi hi)
  | List {values} -> begin match values with
    | [] -> v.visit_list_nil acc
    | h :: t -> let (acc_h, acc_t) = v.enter_list_cons acc in
      v.exit_list_cons (expression_visit_helper v acc_h h)
                       (expression_visit_helper v acc_t (List {values = t}))
      end
  | Record r -> v.visit_record acc r
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
  | TypeMember tm -> v.visit_type_member acc tm
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
    let acc' = v.enter_function_call acc type_args args in
    v.exit_function_call (expression_visit_helper v acc' func)
  | NamelessInstantiation ni -> v.visit_nameless_instantiation ni
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

(* TODO Declaration is recursive *)

type ('a, 'b) declaration_visitor = {
  visit_constant:
    'a ->
    { annotations: Annotation.t list;
      typ: Type.t [@key "type"];
      name: P4String.t;
      value: Expression.t } -> 'b;
  visit_instantiation:
    'a ->
    { annotations: Annotation.t list;
      typ: Type.t [@key "type"];
      args: Argument.t list;
      name: P4String.t;
      init: Block.t option; } -> 'b;
  (* TODO How to enter a recursive list?
  Could have enter nil and enter cons
  Correspondingly, exit nil and exit cons
  Is the representation here redundant?
  *)
  (*
  enter_parser:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      type_params: P4String.t list;
      params: Parameter.t list;
      constructor_params: Parameter.t list;
      locals: t list;
      states: Parser.state list } -> 'a list;
  exit_parser: 'b list -> 'b;
  *)
  (* leaving out side information for now *)
  visit_parser_nil: 'a -> 'b;
  enter_parser_cons: 'a -> ('a * 'a);
  exit_parser_cons: 'b -> 'b -> 'b;
  visit_control_nil: 'a -> 'b;
  enter_control_cons: 'a -> ('a * 'a);
  exit_control_cons: 'b -> 'b -> 'b;
  visit_function:
    'a ->
    { return: Type.t;
      name: P4String.t;
      type_params: P4String.t list;
      params: Parameter.t list;
      body: Block.t } -> 'b;
  visit_extern_function:
    'a ->
    { annotations: Annotation.t list;
      return: Type.t;
      name: P4String.t;
      type_params: P4String.t list;
      params: Parameter.t list } -> 'b;
  visit_variable:
    'a ->
    { annotations: Annotation.t list;
      typ: Type.t [@key "type"];
      name: P4String.t;
      init: Expression.t option } -> 'b;
  visit_value_set:
    'a ->
    { annotations: Annotation.t list;
          typ: Type.t [@key "type"];
          size: Expression.t;
          name: P4String.t } -> 'b;
  visit_action:
    'a ->
    { annotations: Annotation.t list;
          name: P4String.t;
          params: Parameter.t list;
          body: Block.t } -> 'b;
  visit_table:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      properties: Table.property list } -> 'b:
  visit_header:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      fields: Declaration.field list } -> 'b;
  visit_header_union:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      fields: Declaration.field list } -> 'b;
  visit_struct:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      fields: Declaration.field list } -> 'b;
  visit_error: 'a -> { members: P4String.t list } -> 'b;
  visit_match_kind: 'a -> { members: P4String.t list } -> 'b;
  visit_enum:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      members: P4String.t list } -> 'b;
  visit_serializable_enum:
    'a ->
    { annotations: Annotation.t list;
      typ: Type.t [@key "type"];
      name: P4String.t;
      members: (P4String.t * Expression.t) list } -> 'b;
  visit_extern_object:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      type_params: P4String.t list;
      methods: MethodPrototype.t list } -> 'b;
  (* TODO these next two groups of cases ignore side information *)
  visit_type_def_type: 'a -> Type.t -> 'b;
  enter_type_def_decl: 'a -> 'a;
  exit_type_def_decl: 'b -> 'b;
  visit_new_type_type: 'a -> Type.t -> 'b;
  enter_new_type_decl: 'a -> 'a;
  exit_new_type_decl: 'b -> 'b;
  visit_control_type:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      type_params: P4String.t list;
      params: Parameter.t list } -> 'b;
  visit_parser_type:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      type_params: P4String.t list;
      params: Parameter.t list } -> 'b;
  visit_package_type:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      type_params: P4String.t list;
      params: Parameter.t list } -> 'b;
}

let rec declaration_visit_helper v acc d_info =
  match snd d_info with
  | Constant c -> v.visit_constant acc c
  | Instantiation i -> v.visit_instantiation acc i
  | Parser _ -> failwith "TODO"
  | Control _ -> failwith "TODO"
  | Function f -> v.visit_function acc f
  | ExternFunction ef -> v.visit_extern_function acc ef
  | Variable v -> v.visit_variable acc v
  | ValueSet vs -> v.visit_value_set acc vs
  | Action of
      { annotations: Annotation.t list;
        name: P4String.t;
        params: Parameter.t list;
        body: Block.t }
  | Table of
      { annotations: Annotation.t list;
        name: P4String.t;
        properties: Table.property list }
  | Header of
      { annotations: Annotation.t list;
        name: P4String.t;
        fields: field list }
  | HeaderUnion of
      { annotations: Annotation.t list;
        name: P4String.t;
        fields: field list }
  | Struct of
      { annotations: Annotation.t list;
        name: P4String.t;
        fields: field list }
  | Error of
      { members: P4String.t list }
  | MatchKind of
      { members: P4String.t list }
  | Enum of
      { annotations: Annotation.t list;
        name: P4String.t;
        members: P4String.t list }
  | SerializableEnum of
      { annotations: Annotation.t list;
        typ: Type.t [@key "type"];
        name: P4String.t;
        members: (P4String.t * Expression.t) list }
  | ExternObject of
      { annotations: Annotation.t list;
        name: P4String.t;
        type_params: P4String.t list;
        methods: MethodPrototype.t list }
  | TypeDef of
      { annotations: Annotation.t list;
        name: P4String.t;
        typ_or_decl: (Type.t, t) alternative }
  | NewType of
      { annotations: Annotation.t list;
        name: P4String.t;
        typ_or_decl: (Type.t, t) alternative }
  | ControlType of
      { annotations: Annotation.t list;
        name: P4String.t;
        type_params: P4String.t list;
        params: Parameter.t list }
  | ParserType of
      { annotations: Annotation.t list;
        name: P4String.t;
        type_params: P4String.t list;
        params: Parameter.t list }
  | PackageType of
      { annotations: Annotation.t list;
        name: P4String.t;
        type_params: P4String.t list;
        params: Parameter.t list }

(*
TODO Left 'a and Right 'b are the alternative constructors

What does the "info" mean?
Also, why the "pre" distinction?
*)

(* TODO Statement is recursive *)

type ('a, 'b) statement_visitor = {
  visit_method_call:
    'a ->
    { func: Expression.t;
      type_args: Type.t list;
      args: Argument.t list } -> 'b;
  visit_assignment:
    'a ->
    { lhs: Expression.t;
      rhs: Expression.t } -> 'b;
  visit_direct_application:
    'a ->
    { typ: Type.t;
      args: Argument.t list } -> 'b;
  (* the right member of the output is unused in the None case *)
  enter_conditional: 'a -> Expression.t -> ('a * 'a);
  exit_conditional: 'b -> 'b option -> 'b;
  visit_block_statement: 'a -> { block: Block.t } -> 'b;
  visit_exit: 'a -> 'b;
  visit_empty_statement: 'a -> 'b;
  visit_return: 'a -> { expr: Expression.t option } -> 'b;
  visit_switch:
    'a ->
    { expr: Expression.t;
      cases: switch_case list } -> 'b;
  visit_declaration_statement: 'a -> { decl: Declaration.t } -> 'b;
}

let rec statement_visit_helper v acc s_info =
  match snd s_info with
  | MethodCall mc -> v.visit_method_call acc mc
  | Assignment a -> v.visit_assignment acc a
  | DirectApplication da -> v.visit_direct_application acc da
  | Conditional {cond; tru; fls} ->
    let (acc_tru, acc_fls) = v.enter_conditional acc cond in
    begin match fls with
      | None -> v.exit_conditional
        (statement_visit_helper v acc_tru tru) None
      | Some fls_branch -> v.exit_conditional
        (statement_visit_helper v acc_tru tru)
        (statement_visit_helper v acc_fls fls_branch)
    end
  | BlockStatement bs -> v.visit_block_statement acc bs
  | Exit -> v.visit_exit acc
  | EmptyStatement -> v.visit_empty_statement acc
  | Return r -> v.visit_return acc r
  | Switch s -> v.visit_switch acc s
  | DeclarationStatement ds -> v.visit_declaration_statement acc ds

(*
TODO Block not recursive
Blocks contain annotations and statements
Declarations and statements contain blocks
Annotations contain expressions and Key Values
*)

(* TODO Program not recursive *)

type ('a, 'b) program_visitor = {
  visit_program_nil: 'a -> 'b;
  enter_program_cons: 'a -> ('a * 'a);
  exit_program_cons: 'b -> 'b -> 'b;
}

(**
  The Program type does not contain info.
*)
let rec program_visit_helper v acc = function
  | Program [] -> v.visit_program_nil acc
  | Program (h :: t) -> let (acc_h, acc_t) = v.enter_program_cons acc in
    v.exit_program_cons () () (* TODO *)

(* Example uses of the visitor structures *)

(**
  This visitor determines the depth of a Type.t.  It does not take the
  mutual recursion of Petr4 AST types into account:  it treats any
  Expression.t or P4String.t within the Type.t as having no depth.
 *)
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

(**
  This visitor determines the number of nodes in a Statement.t.  It ignores
  all other Petr4 AST types.
*)
let statement_count_visitor =
  let base = (fun _ -> 1) in
  let base_ignore_term = (fun _ _ -> 1) in
  let split = (fun _ -> ((), ())) -> in {
  visit_method_call = base_ignore_term;
  visit_assignment = base_ignore_term;
  visit_direct_application = base_ignore_term;
  enter_conditional = split;
  exit_conditional =
    (fun n1 n2 ->
    match n2 with
    | None -> n1 + 1
    | Some n2' -> n1 + n2' + 1)
  visit_block_statement = base_ignore_term;
  visit_exit = base;
  visit_empty_statement = base;
  visit_return = base_ignore_term;
  visit_switch = base_ignore_term;
  visit_declaration_statement = base_ignore_term;
}

let statement_count =
  statement_visit_helper statement_count_visitor ()

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
let declaration_headers_visitor =
  let get_header_name = (fun h -> [name h]) in
  let base_ignore = (fun _ _ -> []) in
  let enter_one = (fun _ -> ()) in
  let exit_one = (fun l -> l) in {
  visit_constant = base_ignore;
  (* This one can contain a Block *)
  visit_instantiation = failwith "TODO";
  (* TODO implementation not decided *)
  visit_parser_nil: 'a -> 'b;
  enter_parser_cons: 'a -> ('a * 'a);
  exit_parser_cons: 'b -> 'b -> 'b;
  visit_control_nil: 'a -> 'b;
  enter_control_cons: 'a -> ('a * 'a);
  exit_control_cons: 'b -> 'b -> 'b;
  (* TODO can contain a Block *)
  visit_function = failwith "TODO";
  visit_extern_function = base_ignore;
  visit_variable = base_ignore;
  visit_value_set:
    'a ->
    { annotations: Annotation.t list;
          typ: Type.t [@key "type"];
          size: Expression.t;
          name: P4String.t } -> 'b;
  visit_action:
    'a ->
    { annotations: Annotation.t list;
          name: P4String.t;
          params: Parameter.t list;
          body: Block.t } -> 'b;
  visit_table:
    'a ->
    { annotations: Annotation.t list;
      name: P4String.t;
      properties: Table.property list } -> 'b:
  visit_header = get_header_name;
  visit_header_union = get_header_name;
  (* TODO fields are probably irrelevant *)
  visit_struct = base_ignore;
  visit_error = base_ignore;
  visit_match_kind = base_ignore;
  visit_enum = base_ignore;
  visit_serializable_enum = base_ignore;
  visit_extern_object = base_ignore;
  (* TODO these next two groups of cases ignore side information *)
  visit_type_def_type = base_ignore;
  enter_type_def_decl = enter_one;
  exit_type_def_decl = exit_one;
  visit_new_type_type = base_ignore;
  enter_new_type_decl = enter_one;
  exit_new_type_decl = exit_one;
  visit_control_type = base_ignore;
  visit_parser_type = base_ignore;
  visit_package_type = base_ignore;
}

(* TODO need to match on second entry because of info *)