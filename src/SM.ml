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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL  of string * int * bool
(* returns from a function         *) | RET   of bool with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  
let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) = function
| [] -> conf
| insn :: prg' ->
   eval env
     (match insn with
      | BINOP op       -> let y::x::stack' = stack in (cstack, Expr.to_func op x y :: stack', c)
      | READ           -> let z::i'        = i     in (cstack, z::stack, (st, i', o))
      | WRITE          -> let z::stack'    = stack in (cstack, stack', (st, i, o @ [z]))
      | CONST i        -> (cstack, i::stack, c)
      | LD x           -> (cstack, (State.eval st x) :: stack, c)
      | ST x           -> let z::stack'    = stack in (cstack, stack', (State.update x z st, i, o))
      | LABEL l        -> conf
      | JMP l          -> conf
      | CJMP (cond, l) -> let z::stack'    = stack in (cstack, stack', c)
      | BEGIN (_, args, ls) -> 
        let inner_st = State.enter st (args @ ls) in 
        let st', stack' = List.fold_left (fun (s, (z :: stck)) x -> State.update x z s, stck) (inner_st, stack) args in
        (cstack, stack', (st', i, o))
      | END | RET _    ->
        (match cstack with 
        | [] -> conf
        | (_, s) :: cstack' -> (cstack', stack, (State.leave st s, i, o)) 
        )
      | CALL (name, _, _) -> 
        ((prg', st)::cstack, stack, c)
     )
     (let cjmp_cond cond x =
        match cond with
        | "z"  -> x = 0
        | "nz" -> x <> 0
        | _    -> failwith "Unexpected cjump parameter"
      in
        match insn with
        | JMP l             -> env#labeled l
        | CJMP (cond, l)    -> let z::stack' = stack in (if cjmp_cond cond z then (env#labeled l) else prg')
        | CALL (name, _, _) -> env#labeled name
        | END | RET _       -> (match cstack with [] -> [] | (p, _) :: _ -> p)
        | _                 -> prg'
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
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
class label_generator = 
  object (self)
    val cntr = 2
    method create = 
      let str = string_of_int cntr 
      in ".L"^str, {<cntr = cntr + 1>}
end

(*
Might be used for function overloading

let createfunlabel name argcount = 
  let argcountstr = string_of_int argcount
  in name^"("^argcountstr^")"
*)

let rec compile' labgen =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (name, args)->
    let compiled_args = List.fold_right (fun e l -> l @ (expr e)) args [] in
    compiled_args @ [CALL (name, List.length args, true)]
  in
  function
  | Stmt.Read x        -> labgen, [READ; ST x]
  | Stmt.Write e       -> labgen, expr e @ [WRITE]
  | Stmt.Assign (x, e) -> labgen, expr e @ [ST x]
  | Stmt.Skip          -> labgen, []
  | Stmt.If (c, s, es) -> 
      let else_label, labgen = labgen#create in
      let endif_label, labgen = labgen#create in
      let labgen, compiled_s = compile' labgen s in
      let labgen, compiled_es = compile' labgen es in
      labgen, 
      expr c @ [CJMP("z", else_label)]
        @ compiled_s @ [JMP endif_label; LABEL else_label] 
        @ compiled_es
        @ [LABEL endif_label]
  | Stmt.While (c, s)  ->
      let while_label, labgen = labgen#create in
      let endwhile_label, labgen = labgen#create in 
      let labgen, compiled_s = compile' labgen s in
      labgen,
      [LABEL while_label] @ expr c @ [CJMP("z", endwhile_label)]
      @ compiled_s @ [JMP while_label; LABEL endwhile_label]
  | Stmt.Repeat (s, c) ->
      let repeat_label, labgen = labgen#create in
      let labgen, compiled_s = compile' labgen s in
      labgen,
      [LABEL repeat_label] @ compiled_s @ (expr c) @ [CJMP("z", repeat_label)]
  | Stmt.Seq (s1, s2)  -> 
      let labgen, compiled_s1 = compile' labgen s1 in
      let labgen, compiled_s2 = compile' labgen s2 in
      labgen, compiled_s1 @ compiled_s2
  | Stmt.Call (name, args) ->
      let compiled_args = List.fold_left (fun ilist arg -> ilist @ (expr arg)) [] args in
      labgen, compiled_args @ [CALL (name, List.length args, false)]
  | Stmt.Return optexpr    ->
      labgen, 
        (match optexpr with Some e -> expr e | None -> []) @
        [RET (match optexpr with Some _ -> true | None -> false)]

let compile_proc labgen (name, (argnames, locals, body)) = 
  let labgen, compiled_body = compile' labgen body in
  labgen, [LABEL (name); BEGIN (name, argnames, locals)] @ compiled_body @ [END]

let compile (defs, p) = 
  let labgen, compiled_defs = List.fold_left 
    (fun (lg, ilist) def -> 
      let (lg', proc_ilist) = compile_proc lg def in (lg', ilist @ proc_ilist))
    (new label_generator, []) defs in
  let (_, compiled_s) = compile' labgen p in
  compiled_s @ [END] @ compiled_defs
