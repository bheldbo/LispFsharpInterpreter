
// REPL for LISP variant
// Compile with "source compile.sh"
// Run with "mono lisp.exe"

// Type in S-expression at the "> " prompt.
// It may span several lines -- a new prompt is shown for each line.
// When (after an "Enter" is typed) a complete S-expression is found
// it is evaluated and the result shown after a "= ".
// If an error is found during parsing or evaluation,
// a message is displayed after a "! ".
// In both cases, the "> " prompt is shown again.
// Close session by pressing ^D (control-D) at the prompt

open Sexp;;

// Global environment (initially empty)

let mutable globalEnv = []

// look up variable in environment
// environment is represented as list of (name,value) pairs

let rec lookup env x =
  match env with
  | [] -> None
  | ((y,v) :: env1) -> if x=y then Some v else lookup env1 x

// update binding in environment, adding new if not bound
// only used for global environment

let rec update env x v =
  match env with
  | [] -> [(x,v)]
  | ((y,w) :: env1) ->
       if x=y then (x,v) :: env1 else (y,w) :: update env1 x v

// update global environment

let updateGlobal x v =
  globalEnv <- update globalEnv x v

// exception for LISP evaluation errors

exception Lerror of string

// list of keywords
let keywords = ["quote"; "lambda"; "define"; "cons"; "save"; "load"; "+"; "-"; "*"; "/"]

// evaluate Sexp to Sexp in local environment
// if error raise exception Lerror message

let rec eval s localEnv =
  match s with
  | Nil -> Nil
  | Symbol x -> // keyword or variable
      if List.contains x keywords then
        raise (Lerror ("keyword " + x + " can not be used as variable"))
      else
        match lookup (localEnv @ globalEnv) x with
        | Some v -> v
        | None -> raise (Lerror ("Undefined variable " + x))
  | Number value -> Number value // Integers
  | Cons (Symbol "quote", Cons (v, Nil)) -> v
  | Cons (Symbol "lambda", rules) -> s
  | Cons (Symbol "define", Cons (Symbol x, Cons (e, Nil))) ->
      if List.contains x keywords then
        raise (Lerror ("keyword " + x + " can not be redefined"))
      else
        (updateGlobal x (eval e localEnv); Nil)
  | Cons (Symbol "cons", Cons (e1, Cons (e2, Nil))) ->
      Cons (eval e1 localEnv, eval e2 localEnv)
  | Cons (Symbol "save", Cons (Symbol f, Nil)) ->
      try
        let envText = // create definitions as text, one per line
              String.concat "\n"
                (List.map
                  (fun (x,v) -> "(define " + x + " " + quoteExp v + ")")
                   globalEnv)
        let outfile = System.IO.File.CreateText (f + ".le")
        outfile.Write envText; outfile.Close (); Nil
      with _ -> raise (Lerror ("Could not open file " + f + ".le"))
  | Cons (Symbol "load", Cons (Symbol f, Nil)) ->
      try
        let infile = System.IO.File.OpenText (f + ".le")
        while not infile.EndOfStream do // read definitions, one per line
          match readSexp (infile.ReadLine ()) with
          | Success (s, 0) -> ignore (eval s [])
          | _ -> raise (Lerror ("Error while reading " + f + ".le"))
        ; infile.Close (); Nil
      with _ -> raise (Lerror ("Could not open file " + f + ".le"))
  | Cons (Symbol "+", exp) -> // Plus branch >
    match exp with
    | Nil -> Number 0
    | Cons (Number i1, exp) ->
      match eval (Cons (Symbol "+", exp)) localEnv with
      | Number i2 -> Number ((i1) + (i2))
      | _ -> raise (Lerror ("Bad '+' operation"))
    | Cons (Symbol x, e) ->
      match eval (Cons (Symbol "+", e)) localEnv with
      | Number i2 ->
        match lookup (localEnv @ globalEnv) x with
        | Some (Number i1) -> Number ((i1)+(i2))
        | _ -> raise (Lerror ("Bad '+' operation"))
      | _ -> raise (Lerror ("Bad '+' operation"))
    | Cons (x,y) ->
      match (eval x localEnv, eval y localEnv) with
      | (Number i1, Nil) -> Number i1
      | (Number i1, Number i2) -> Number ((i1)+(i2))
      | (Symbol a, Nil) ->
        match lookup (localEnv @ globalEnv) a with
        | Some (Number i1) -> Number (i1)
        | _ -> raise (Lerror ("Bad '+' operation"))
      | (Symbol a, Symbol b) ->
        match (lookup (localEnv @ globalEnv) a, lookup (localEnv @ globalEnv) b) with
        | (Some (Number i1), Some (Number i2)) -> Number ((i1)+(i2))
        |_ -> raise (Lerror ("Bad '+' operation"))
      | (Number i1, Symbol a) | (Symbol a, Number i1) ->
        match lookup (localEnv @ globalEnv) a with
        | Some (Number i2) -> Number ((i1)+(i2))
        | _ -> raise (Lerror ("Bad '+' operation"))
      | _ -> raise (Lerror ("Bad '+' operation"))
    | _ -> raise (Lerror ("Bad '+' operation"))
  | Cons (Symbol "-", exp) -> // Minus branch >
      match exp with
      | Nil -> Number 0
      | Cons (i1, Nil) ->
        match (eval i1 localEnv) with
        | Number i1 -> Number i1
        | _ -> raise (Lerror ("Bad '-' operation"))
      | Cons (Number i1, exp) ->
        match eval (Cons (Symbol "+", exp)) localEnv with
        | Number i2 -> Number ((i1) - (i2))
        | _ -> raise (Lerror ("Bad '-' operation"))
      | Cons (Symbol x, e) ->
        match eval (Cons (Symbol "-", e)) localEnv with
        | Number i2 ->
          match lookup (localEnv @ globalEnv) x with
          | Some (Number i1) -> Number ((i1)-(i2))
          | _ -> raise (Lerror ("Bad '-' operation"))
        | _ -> raise (Lerror ("Bad '-' operation"))
      | Cons (x,y) ->
        match (eval x localEnv, eval y localEnv) with
        | (Number i1, Number i2) -> Number ((i1)-(i2))
        | (Number i1, Nil) -> Number i1
        | (Symbol a, Nil) ->
          match lookup (localEnv @ globalEnv) a with
          | Some (Number i1) -> Number (i1)
          | _ -> raise (Lerror ("Bad '-' operation"))
        | (Symbol a, Symbol b) ->
          match (lookup (localEnv @ globalEnv) a, lookup (localEnv @ globalEnv) b) with
          | (Some (Number i1), Some (Number i2)) -> Number ((i1)-(i2))
          |_ -> raise (Lerror ("Bad '-' operation"))
        | (Number i1, Symbol a) | (Symbol a, Number i1) ->
          match lookup (localEnv @ globalEnv) a with
          | Some (Number i2) -> Number ((i1)-(i2))
          | _ -> raise (Lerror ("Bad '-' operation"))
        | _ -> raise (Lerror ("Bad '-' operation"))
      | _ -> raise (Lerror ("Bad '-' operation"))
  | Cons (Symbol "*", exp) -> // Mult branch >
    match exp with
    | Nil -> Number 1
    | Cons (Number i1, exp) ->
      match eval (Cons (Symbol "*", exp)) localEnv with
      | Number i2 -> Number ((i1) * (i2))
      | _ -> raise (Lerror ("Bad '*' operation"))
    | Cons (x,y) ->
      match (eval x localEnv, eval y localEnv) with
      | (Number i1, Number i2) -> Number ((i1) * (i2))
      | (Number i1, Nil) -> Number i1
      | (Symbol a, Nil) ->
        match lookup (localEnv @ globalEnv) a with
        | Some (Number i1) -> Number (i1)
        | _ -> raise (Lerror ("Bad '*' operation"))
      | (Symbol a, Symbol b) ->
        match (lookup (localEnv @ globalEnv) a, lookup (localEnv @ globalEnv) b) with
        | (Some (Number i1), Some (Number i2)) -> Number ((i1) * (i2))
        |_ -> raise (Lerror ("Bad '*' operation"))
      | (Number i1, Symbol a) | (Symbol a, Number i1) ->
        match lookup (localEnv @ globalEnv) a with
        | Some (Number i2) -> Number ((i1) * (i2))
        | _ -> raise (Lerror ("Bad '*' operation"))
      | _ -> raise (Lerror ("Bad '*' operation"))
    | _ -> raise (Lerror ("Bad '*' operation"))
  | Cons (Symbol "/", exp) -> // Mult branch >
    match exp with
    | Nil -> Number 1
    | Cons (Number i1, exp) ->
      match eval (Cons (Symbol "/", exp)) localEnv with
      | Number 0 -> raise (Lerror ("ERROR: division by zero"))
      | Number i2 -> Number ((i1) / (i2))
      | _ -> raise (Lerror ("Bad '/' operation"))
    | Cons (Symbol x, e) ->
      match eval (Cons (Symbol "/", e)) localEnv with
      | Number i2 ->
        match lookup (localEnv @ globalEnv) x with
        | Some (Number i1) -> Number ((i1) / (i2))
        | _ -> raise (Lerror ("Bad '/' operation"))
      | _ -> raise (Lerror ("Bad '/' operation"))
    | Cons (x,y) ->
      match (eval x localEnv, eval y localEnv) with
      | (Number i1, Nil) -> Number i1
      | (Number i1, Number i2) -> Number ((i1) / (i2))
      | (Symbol a, Nil) ->
        match lookup (localEnv @ globalEnv) a with
        | Some (Number i1) -> Number (i1)
        | _ -> raise (Lerror ("Bad '/' operation"))
      | (Symbol a, Symbol b) ->
        match (lookup (localEnv @ globalEnv) a, lookup (localEnv @ globalEnv) b) with
        | (Some (Number i1), Some (Number i2)) -> Number ((i1) / (i2))
        |_ -> raise (Lerror ("Bad '/' operation"))
      | (Number i1, Symbol a) | (Symbol a, Number i1) ->
        match lookup (localEnv @ globalEnv) a with
        | Some (Number i2) -> Number ((i1) / (i2))
        | _ -> raise (Lerror ("Bad '/' operation"))
      | _ -> raise (Lerror ("Bad '/' operation"))
    | _ -> raise (Lerror ("Bad '/' operation"))
  | Cons (i, Nil) -> i // Plus branch
  | Cons (Symbol x, pars) when List.contains x keywords ->
      raise (Lerror ("malformed " + x + " expression"))
  | Cons (e1, pars) -> // function application
      match eval e1 localEnv with
      | Cons (Symbol "lambda", rules) ->
          tryRules rules (evalList pars localEnv) localEnv
      | v -> raise (Lerror (showSexp v + " can not be applied as a function"))


// evaluate argument list

and evalList es localEnv =
  match es with
  | Nil -> Nil
  | Cons (e1, es1) -> Cons (eval e1 localEnv, evalList es1 localEnv)
  | _ -> raise (Lerror ("arguments are not a list: " + showSexp es))

and tryRules rs args localEnv =
  match rs with
  | Cons (p, Cons (e, rs1)) ->
      match matchPattern p args with
      | Some env -> eval e (env @ localEnv)
      | None -> tryRules rs1 args localEnv
  | Nil -> raise (Lerror ("No patterns matched arguments " + showSexp args))
  | _ ->  raise (Lerror ("Malformed rules " + showSexp rs))

// match pattern to argument list

and matchPattern p v =
  match (p, v) with
  | (Nil, Nil) -> Some []
  | (Symbol x, w) ->
       if List.contains x keywords
       then raise (Lerror ("keyword " + x + " can not be used in pattern"))
       else Some [(x, w)]
  | (Cons (p1, p2), Cons (v1, v2)) ->
       match (matchPattern p1 v1, matchPattern p2 v2) with
       | (Some env1, Some env2) ->
            if disjoint env1 env2 then Some (env1 @ env2) else None
       | _ -> None
  | _ -> None

// check if variable is repeated in pattern by comparing environments

and disjoint env1 env2 =
  match env1 with
  | [] -> true
  | (x,v) :: env3 ->
       match lookup env2 x with
       | Some _ -> raise (Lerror ("repeated variable " + x + " in pattern"))
       | None -> disjoint env3 env2

and quoteExp v =
  match v with
  | Nil -> "()"
  | (Cons (Symbol "quote", _)) -> showSexp v
  | (Cons (Symbol "lambda", _)) -> showSexp v
  | _ -> "(quote " + showSexp v + ")"

// read-eval-print loop (REPL) for LISP variant
// See functionality at top of this file

exception Exit

let rec repl prefix =
  if prefix = "" then printf "> " ;
  let input = System.Console.ReadLine()
  if input = null then raise Exit
  let s = prefix + input
  let len = String.length s
  match Sexp.readSexp s with
  | Success (e, p) ->
     if p=0 then
       (try printf "= %s\n" (showSexp (eval e []))
        with Lerror mess -> printf "! %s \n" mess)
     else
       printf "! %s\n" "Input is not a single S-expression"
     ; repl ""
  | ErrorAt i ->
      if i=len then
        repl s
      else
       printf "! %s \n" ("parse error at position " + string i);
       repl ""

// Start REPL

printf "PLD LISP v. 1.0\n"; try repl "" with Exit -> printf "\n"
