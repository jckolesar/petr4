module I = Info
module Info = I (* JNF: ugly hack *)
open Env
open Types
open Value

(*TODO: this should be string*)
val eval_program: program -> ctrl -> packet_in -> string
val print_eval_program: program -> ctrl -> packet_in -> unit
val eval_expression: EvalEnv.t -> ctrl -> Expression.t -> EvalEnv.t * value

val eval_app: EvalEnv.t -> ctrl -> signal -> value -> Argument.t list -> (EvalEnv.t * signal * value)
val eval_assign': EvalEnv.t -> ctrl -> lvalue -> value -> EvalEnv.t * signal
val init_val_of_typ: EvalEnv.t -> ctrl -> string -> Type.t -> value
val eval_decl: EvalEnv.t -> ctrl -> Declaration.t -> EvalEnv.t
