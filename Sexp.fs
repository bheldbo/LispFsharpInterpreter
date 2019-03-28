// Sexp.fs: S-expressions for LISP variant

module Sexp

// Type for S-expressions

type Sexp = Symbol of string
          | Nil
          | Cons of Sexp * Sexp
          | Number of int // Integers

// Convert Sexp to string

let rec showSexp s =
  match s with
  | Symbol name -> name
  | Nil  -> "()"
  | Cons (Symbol "quote", Cons (s, Nil)) -> "'" + showSexp s
  | Cons (a,d) -> "(" + showSexp a + showTail d
  | Number value -> (string value) // Integers



and showTail d =
  match d with
  | Symbol name -> " . " + name + ")"
  | Nil  -> ")"
  | Cons (a,d1) -> " " + showSexp a + showTail d1
  | Number value -> " . " + (string value) + ")" // Integers


// Type for success or failure for parsing

type ParseResult = Success of Sexp * int // expression and position
                 | ErrorAt of int // position


// Read Sexp from string
// Return Success (s, 0) if input matches an Sexp s with no "junk" afterwards
// Return Success (s, i) if prefix of input matches,
// but there is "junk" at position i
// Return ErrorAt i if a parse error is found at position i

let rec readSexp cs =
  let len = String.length cs
  match readExp cs 0 len with
  | Success (s, i) ->
      let j = skipWhite cs i len
      if j=len then Success (s, 0) else Success (s, j)
  | ErrorAt i -> ErrorAt i

// skip whitespace

and skipWhite cs i len  =
  if i < len && (cs.[i]  = ' ' || cs.[i] = '\n')
  then skipWhite cs (i+1) len
  else i

// read Sexp from position i in string
// and return either ErrorAt j (if string didn't match at position j) or
// Success (s, j) where s is the read Sexp and j is position after read Sexp

and readExp cs i len =
  if i = len then ErrorAt i
  else
    let c = cs.[i]
    if 'a' <= c &&  c <= 'z' then // read symbol
      readSymbol (string c) cs (i+1) len
    else if '0' <= c && c <= '9' then // Integers
      readInt (int c) cs (i+1) len
    else if c = '(' then // this starts a list or Nil, read the rest
      readTail cs (i+1) len
    else if c = '\'' then // read quoted expression
      match readExp cs (i+1) len with
      | Success (s, j) -> Success (Cons (Symbol "quote", Cons (s, Nil)), j)
      | other -> other
    else if c = ' ' || c = '\n' then // skip whitespace
      readExp cs (i+1) len
    else ErrorAt i

// Integers
// read int
// add number to exp (already read number)
// stop when end of string or non-letter reached

and readInt value cs i len =
  if i = len then Success (Number (value-48), i)
  else
    let c = cs.[i]
    if '0' <= c && c <= '9' then
      readInt ((value-48)*10 + (int c)) cs (i+1) len // Ascii table translation for value 48..58 = 0..9
    else Success (Number (value-48), i)

// read symbol (all lower case letters)
// add letters to name (already read letters)
// stop when end of string or non-letter reached

and readSymbol name cs i len =
  if i = len then Success (Symbol name, i)
  else
    let c = cs.[i]
    if 'a' <= c &&  c <= 'z' then readSymbol (name + string c) cs (i+1) len
    else Success (Symbol name, i)

// read list tail
// handles shorthand (a1 a2 ... an) = (a1 . (a2 . ... an))

and readTail cs i len  =
  if i = len then ErrorAt i
  else
    let c = cs.[i]
    if c = ')' then Success (Nil, i+1)  // end of list
    else if 'a' <= c &&  c <= 'z' || c = '+' || c = '-' then  // read symbol and then tail // Plus branch
      match readSymbol (string c) cs (i+1) len with
      | ErrorAt j -> ErrorAt j
      | Success (sym, j) ->
         match readTail cs j len with
         | ErrorAt j -> ErrorAt j
         | Success (list, j) -> Success (Cons (sym, list), j)
    else if c = '(' then // read Sexp and then tail
      match readTail cs (i+1) len with
      | ErrorAt j -> ErrorAt j
      | Success (s, j) ->
         match readTail cs j len with
         | ErrorAt j -> ErrorAt j
         | Success (list, j) -> Success (Cons (s, list), j)
    else if c = '\'' then // read 'Sexp and then tail
      match readExp cs (i+1) len with
      | Success (s, j) ->
         match readTail cs j len with
         | ErrorAt j -> ErrorAt j
         | Success (list, j) ->
             Success (Cons (Cons (Symbol "quote", Cons (s, Nil)), list), j)
      | other -> other
    else if c = '.' then // read Sexp and then ")"
      match readExp cs (i+1) len with
      | ErrorAt j -> ErrorAt j
      | Success (s, j) ->
         match readClose cs j len with
         | ErrorAt j -> ErrorAt j
         | Success (_, j) -> Success (s, j)
    else if c = ' ' || c = '\n' then // skip whitespace
      readTail cs (i+1) len
    else if '0' <= c &&  c <= '9' then  // Integers read number and then tail
      match readInt (int c) cs (i+1) len with
      | ErrorAt j -> ErrorAt j
      | Success (sym, j) ->
         match readTail cs j len with
         | ErrorAt j -> ErrorAt j
         | Success (list, j) -> Success (Cons (sym, list), j)
    else ErrorAt i


// read close parenthesis (possibly preceeded by whitespace)

and readClose cs i len  =
  if i = len then ErrorAt i
  else
    let c = cs.[i]
    if c = ')' then Success (Nil, i+1)
    else if c = ' ' || c = '\n' then readClose cs (i+1) len
    else ErrorAt i
