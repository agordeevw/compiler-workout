(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Generate binary operator from operator string description. *)
    let binop opstring =
      let bool_to_int b = if b then 1 else 0 
      and int_to_bool i = i != 0
      in (
        let boolbinop = fun boolop l r -> bool_to_int (boolop (int_to_bool l) (int_to_bool r))
        and compbinop = fun compop l r -> bool_to_int (compop l r)
        in match opstring with
          | "!!" -> boolbinop (||)
          | "&&" -> boolbinop (&&)
          | "==" -> compbinop (= )
          | "!=" -> compbinop (<>)
          | "<=" -> compbinop (<=)
          | "<"  -> compbinop (< )
          | ">=" -> compbinop (>=)
          | ">"  -> compbinop (> )
          | "+"  -> ( + )
          | "-"  -> ( - )
          | "*"  -> ( * )
          | "/"  -> ( / )
          | "%"  -> ( mod )
          | _    -> failwith "Not an operator"
        )

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let rec eval _state _expr =
    match _expr with
    | Const value -> value
    | Var   name  -> _state name
    | Binop (opstring, lhs, rhs) ->
        let lhsvalue = eval _state lhs
        and rhsvalue = eval _state rhs
        in binop opstring lhsvalue rhsvalue

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    ostap (
      parse: empty {failwith "Not implemented yet"}
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (state, istream, ostream) stmt =
    match stmt with
    | Assign (varname, expression) -> 
        let new_state = Expr.update varname (Expr.eval state expression) state
        in (new_state, istream, ostream)
    | Read varname -> (
        match istream with
        | value :: tail_istream ->
            let new_state = Expr.update varname value state
            in (new_state, tail_istream, ostream)
        | [] -> failwith("Empty input stream")
        )
    | Write expression ->
        let value = Expr.eval state expression
        in (state, istream, ostream @ [value])
    | Seq (fst_stmt, snd_stmt) ->
        let fst_state = eval (state, istream, ostream) fst_stmt
        in eval fst_state snd_stmt

    (* Statement parser *)
    ostap (
      parse: empty {failwith "Not implemented yet"}
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
