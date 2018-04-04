(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    let is_local x s = List.exists (fun z -> x = z) s.scope

    (* Empty state *)
    let empty' = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)
    let empty = {g = empty'; l = empty'; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s = 
      let update' x v s = fun y -> if x = y then v else s y in 
      if (is_local x s) 
        then {g = s.g; l = update' x v s.l; scope = s.scope}
        else {g = update' x v s.g; l = s.l; scope = s.scope} 
                                
    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = if (is_local x s) then s.l x else s.g x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {g = st.g; l = empty'; scope = xs}

    (* Drops a scope *)
    let leave st st' = {g = st.g; l = st'.l; scope = st'.scope}

  end
    
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
      | Var   x -> State.eval st x
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
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters, local variables, and a body for given definition
    *)
    let rec eval env ((st, i, o) as conf) stmt =
      match stmt with
      | Read    x           -> (match i with z::i' -> (State.update x z st, i', o) | _ -> failwith "Unexpected end of input")
      | Write   e           -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e)       -> (State.update x (Expr.eval st e) st, i, o)
      | Seq    (s1, s2)     -> eval env (eval env conf s1) s2
      | Skip                -> conf
      | If     (c, s1, s2)  -> eval env conf (if Expr.eval st c <> 0 then s1 else s2)
      | While  (c, s)       -> if Expr.eval st c = 1 then eval env (eval env conf s) (While (c, s)) else conf
      | Repeat (s, c)       -> let (st', i', o') = eval env conf s in 
        (if Expr.eval st' c = 0 then eval env (st', i', o') (Repeat (s, c)) else (st', i', o'))
      | Call (name, vals)   -> 
        let argnames, locals, body = env#definition name in
        let inner_st = State.enter st (argnames @ locals) in
        let state_update = (fun s a v -> State.update a (Expr.eval st v) s) in
        let inner_st1 = List.fold_left2 state_update inner_st argnames vals in
        let inner_st2, i', o' = eval env (inner_st1, i, o) body in
        let outer_st = State.leave inner_st2 st in
        (outer_st, i', o')


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
        %"until" c:!(Expr.parse)         {Repeat(s, c)}
      | %"for" is:parse 
        "," c:!(Expr.parse)
        "," ss:parse 
        %"do" s:parse %"od"              {Seq(is, While(c, Seq(s, ss)))}
      | x:IDENT "(" args:!(Ostap.Util.listBy)[ostap (",")][Expr.parse]? ")"
        { Call(x, match args with | Some s -> s | None -> [])}
    )

  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      parse: %"fun" name:IDENT "(" argnames:!(Ostap.Util.listBy)[ostap (",")][ostap (IDENT)]? ")"
         locals:(%"local" !(Ostap.Util.listBy)[ostap (",")][ostap (IDENT)] | empty { [] } ) "{" body:!(Stmt.parse)? "}"
        { name, (
          (match argnames with Some s -> s | None -> []), 
          locals, 
          (match body with Some s -> s | None -> Skip)) }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let find procname = 
    match (List.find_opt (fun n, _ -> n = procname) defs) with
    | Some (_, def) -> def
    | None -> failwith "Undefined procedure"
  in let (_, _, o) = Stmt.eval (object method definition = find end) (State.empty, i, []) body 
  in o
                             
(* Top-level parser *)
let parse = ostap (
  defs:!(Definition.parse)* body:!(Stmt.parse) {defs, body}
)
