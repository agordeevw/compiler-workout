open GT       
       
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
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                      
let eval (_stack, (_state, _istream, _ostream)) prg = 
  let _stmtcfg = (_state, _istream, _ostream)
  in match prg with
  | [] -> (_stack, _stmtcfg)
  | _insn :: _  -> (
    match _insn with
    | BINOP opname -> (
      match _stack with
      | y :: x :: tail_stack -> ((Syntax.Expr.binop opname x y) :: tail_stack, _stmtcfg)
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
      | value :: tail_stack -> (tail_stack, (Syntax.Expr.update varname value _state, _istream, _ostream))
      | _ -> failwith ("ST failure: empty stack")
    )
  )
  

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let compile _ = failwith "Not yet implemented"
