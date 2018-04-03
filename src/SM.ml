open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env ((stack, ((st, i, o) as c)) as conf) = function
| [] -> conf
| insn :: prg' ->
   eval env
     (match insn with
      | BINOP op       -> let y::x::stack' = stack in (Expr.to_func op x y :: stack', c)
      | READ           -> let z::i'        = i     in (z::stack, (st, i', o))
      | WRITE          -> let z::stack'    = stack in (stack', (st, i, o @ [z]))
      | CONST i        -> (i::stack, c)
      | LD x           -> (st x :: stack, c)
      | ST x           -> let z::stack'    = stack in (stack', (Expr.update x z st, i, o))
      | LABEL _        -> conf
      | JMP l          -> conf
      | CJMP (cond, l) -> let z::stack'    = stack in (stack', c)
     )
     (let cjmp_cond cond x =
        match cond with
        | "z"  -> x = 0
        | "nz" -> x <> 0
        | _    -> failwith "Unexpected cjump parameter"
      in
        match insn with
        | JMP l           -> env#labeled l
        | CJMP (cond, l)  -> let z::stack' = stack in (if cjmp_cond cond z then (env#labeled l) else prg')
        | _               -> prg'
     )

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
class label_generator = 
  object (self)
    val if_cntr = 0
    val while_cntr = 0
    val repeat_cntr = 0
    method make_if = 
      let str = string_of_int if_cntr 
      in "else_"^str, "endif_"^str, {<if_cntr = if_cntr + 1>}
    method make_while = 
      let str = string_of_int while_cntr
      in "while_"^str, "endwhile_"^str, {<while_cntr = while_cntr + 1>}
    method make_repeat = 
      let str = string_of_int repeat_cntr
      in "repeat_"^str, {<repeat_cntr = repeat_cntr + 1>}
end

let rec compile' labgen =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Read x        -> labgen, [READ; ST x]
  | Stmt.Write e       -> labgen, expr e @ [WRITE]
  | Stmt.Assign (x, e) -> labgen, expr e @ [ST x]
  | Stmt.Skip          -> labgen, []
  | Stmt.If (c, s, es) -> 
      let else_label, endif_label, labgen1 = labgen#make_if in
      let labgen2, compiled_s = compile' labgen1 s in
      let labgen3, compiled_es = compile' labgen2 es in
      labgen3, 
      expr c @ [CJMP("z", else_label)]
        @ compiled_s @ [JMP endif_label; LABEL else_label] 
        @ compiled_es
        @ [LABEL endif_label]
  | Stmt.While (c, s)  ->
      let while_label, endwhile_label, labgen1 = labgen#make_while in
      let labgen2, compiled_s = compile' labgen1 s in
      labgen2,
      [LABEL while_label] @ expr c @ [CJMP("z", endwhile_label)]
      @ compiled_s @ [JMP while_label; LABEL endwhile_label]
  | Stmt.Repeat (c, s) ->
      let repeat_label, labgen1 = labgen#make_repeat in
      let labgen2, compiled_s = compile' labgen1 s in
      labgen2,
      [LABEL repeat_label] @ compiled_s @ (expr c) @ [CJMP("z", repeat_label)]
  | Stmt.Seq (s1, s2)  -> 
      let labgen1, compiled_s1 = compile' labgen s1 in
      let labgen2, compiled_s2 = compile' labgen1 s2 in
      labgen2, compiled_s1 @ compiled_s2

let compile s = 
  let (_, compiled_s) = compile' (new label_generator) s
  in compiled_s
