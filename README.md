dependencies
fsharp 4 or above https://fsharp.org/use/windows/

linux/mono session ->
$bash compile.sh
$mono lisp.exe


An example session using this LISP REPL is shown below
$ mono lisp.exe
PLD LISP v. 1.0 
> (load listfunctions) 
= () 
> (append ’(a b c) ’(d e f)) 
= (a b c d e f) 
>(+ (-3(-2)
