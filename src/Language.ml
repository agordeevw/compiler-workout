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

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
                                                           
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (
      parse:
      !(Ostap.Util.expr 
        (fun x -> x)
        (
          Array.map (
            fun (a, s) -> a, 
            List.map (
              fun s -> ostap(- $(s)), 
              (fun x y -> Binop (s, x, y))
            ) s
          )
          [|
            `Lefta, ["!!"];
            `Lefta, ["&&"];
            `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
            `Lefta, ["+" ; "-"];
            `Lefta, ["*" ; "/"; "%"];
          |] 
        )
       primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
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
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of Expr.t * t  with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval ((st, i, o) as conf) stmt =
      match stmt with
      | Read    x           -> (match i with z::i' -> (Expr.update x z st, i', o) | _ -> failwith "Unexpected end of input")
      | Write   e           -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e)       -> (Expr.update x (Expr.eval st e) st, i, o)
      | Seq    (s1, s2)     -> eval (eval conf s1) s2
      | Skip                -> conf
      | If     (c, s1, s2)  -> eval conf (if Expr.eval st c = 1 then s1 else s2)
      | While  (c, s)       -> if Expr.eval st c = 1 then eval (eval conf s) (While (c, s)) else conf
      | Repeat (c, s)       -> let (st', i', o') = eval conf s in 
        (if Expr.eval st' c = 0 then eval (st', i', o') (Repeat (c, s)) else (st', i', o'))

    (* Statement parser *)
    let nested_elifs elifs els =
      let last = 
        match els with
        | Some s -> s
        | None   -> Skip
      in List.fold_right (fun (c, s) ss -> If (c, s, ss)) elifs last

    ostap (
      parse:
        s:stmt ";" ss:parse {Seq (s, ss)}
      | stmt;
      stmt:
        %"read" "(" x:IDENT ")"          {Read x}
      | %"write" "(" e:!(Expr.parse) ")" {Write e}
      | x:IDENT ":=" e:!(Expr.parse)     {Assign (x, e)}
      | %"skip"                          {Skip}
      | %"if" c:!(Expr.parse) 
        %"then" ts:!(parse)
        elifs:(%"elif" !(Expr.parse) %"then" parse)*
        els:(%"else" parse)? 
        %"fi"                            {If (c, ts, nested_elifs elifs els)}
      | %"while" c:!(Expr.parse) 
        %"do" s:parse %"od"              {While(c, s)}
      | %"repeat" s:parse
        %"until" c:!(Expr.parse)         {Repeat(c, s)}
      | %"for" is:parse 
        "," c:!(Expr.parse)
        "," ss:parse 
        %"do" s:parse %"od"              {Seq(is, While(c, Seq(s, ss)))}                         
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
