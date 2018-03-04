open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval cfg prg = 
  let run_insn (_stack, (_state, _istream, _ostream)) insn = (
    let _stmtcfg = (_state, _istream, _ostream)
    in match insn with
    | BINOP opname -> (
      match _stack with
      | y :: x :: tail_stack -> ((Expr.binop opname x y) :: tail_stack, _stmtcfg)
      | _ -> failwith ("BINOP failure: Not enough arguments, expected 2")
    )
    | CONST value -> (
      (value :: _stack, _stmtcfg)
    )
    | READ -> (
      match _istream with
      | value :: tail_istream -> (value :: _stack, (_state, tail_istream, _ostream))
      | _ -> failwith ("READ failure: empty istream")
    )
    | WRITE -> (
      match _stack with
      | value :: tail_stack -> (tail_stack, (_state, _istream, _ostream @ [value]))
      | _ -> failwith ("WRITE failure: empty stack")
    )
    | LD varname -> (
      ((_state varname) :: _stack, _stmtcfg)
    )
    | ST varname -> (
      match _stack with
      | value :: tail_stack -> (tail_stack, (Expr.update varname value _state, _istream, _ostream))
      | _ -> failwith ("ST failure: empty stack")
    )
  )
  in match prg with
  | [] -> cfg
  | _insn :: tail_prg -> eval (run_insn cfg _insn) tail_prg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec exprcompile expr =
  match expr with
  | Expr.Const value -> [CONST value]
  | Expr.Var varname -> [LD varname]
  | Expr.Binop (opname, lhs, rhs) -> (exprcompile lhs) @ (exprcompile rhs) @ [BINOP opname]

let rec compile stmt = 
  match stmt with
  | Stmt.Assign (varname, expr) -> (exprcompile expr) @ [ST varname]
  | Stmt.Read varname -> [READ; ST varname]
  | Stmt.Write expr -> (exprcompile expr) @ [WRITE]
  | Stmt.Seq (lstmt, rstmt) -> (compile lstmt) @ (compile rstmt)                         
