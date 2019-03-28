Extending an Interpreter for a Variant of LISP
We consider a variant of the LISP language. 
The values that this LISP variant uses are S-expressions: S → symbol S → () S → (S .S) 
Where a symbol is any non-empty sequence of lower-case letters.
() represents the empty list, and (s1 .s2) represents a list that starts with s1 and has the tail s2.
We will, when reading and printing S-expressions, use the usual LISP shorthand: 
(s1 s2 ··· sn) means the same as (s1 .(s2 ··· (sn .()))). For example, (car x) is short for (car.(x.())).
The list notation can be combined with the dot notation, so (a b . c) is short for (a . (b . c)). 
dditionally, we use the shorthand ’s to mean (quote s).
While S-expressions are values, they are also expressions that can be evaluated.
Evaluation uses the following rules: 
• () evaluates to itself 
• A symbol x evaluates to whatever x is bound to in the current environment, 
  which consists of the global environment extended with local bindings. 
• (quote s) evaluates to s. 
• (define x e) evaluates e to s, binds x to s in the global environment, and returns ().
  If x is already bound, the new binding overwrites the old. 
• (cons e1 e2) evaluates e1 to a and e2 to d and returns (a . d). 
• (save f) saves the current global environment in the ﬁle f.le. 
  The saved environment is represented as a list of deﬁnitions (using define), one per line, so it is fairly readable. 
• (load f) loads a previously saved global environment from the ﬁle f.le. 
  The loaded environment extends the current global environment.
  If the ﬁle does not contain a previously saved environment, the behaviour is undeﬁned. 
• (lambda p1 e1 ··· pn en) evaluates to itself. Note that this implies dynamic binding, as no closure is built. 
• (e0 e1 ··· em) ﬁrst evaluates all ei to si. If s0 = (lambda p1 e1 ··· pn en) and p1 matches (s1 ··· sm) yielding an environment ρ,
  then the current local environment is extended with ρ, and e1 is evaluated in this extended environment. 
  If not, p2 is matched against (s1 ··· sm), and so on, until a pattern matches and the following expression is evaluated. 
  If no pattern matches, a runtime error is reported. 
  
Note that this is similar to the behaviour when applying a F# expression of the form function p1 -> e1 | ...| pn -> en.
  
Pattern matching uses the following rules: 
• The pattern () matches the value () and yields the empty environment. 
• A pattern that is a symbol x matches any value s and yields an environmen that binds x to s. 
• A pattern (p1 .p2) matches a value (s1 .s2) 
  if p1 matches s1 yielding the environment ρ1, p2 matches s2 yielding the environment ρ2,
  and no variable is bound in both ρ1 and ρ2. The matching returns the combined environment ρ1 ∪ρ2. 
• In all other cases, the pattern does not match the value. 

Note that a function car that takes the head of a list can be deﬁned by the expression
(define car (lambda ((a.d)) a)), 
and append can be deﬁned by
(define append (lambda (() bs) bs
                       ((a . as) bs) (cons a (append as bs))))

Note that this uses two rules, one for the empty list and one for the non-empty list. 
In LISP, it is common to deﬁne a function cadr such that (cadr x) ≡ (car (cdr x)), and similar for caar, cdar, and cddr, 
and in many cases also for longer sequences of as and ds. 
We can deﬁne cadr directly by the deﬁnition (define cadr (lambda ((a ad . dd)) ad)).
The ﬁle LISP.zip contains an interpreter written in F# for this LISP variant. 
The interpreter implements a read-eval-print loop (REPL) and starts with an empty global environment. 
See brief instructions for compiling and running at the start of the ﬁle RunLISP.fsx. 
The ﬁle listfunctions.le contains deﬁnitions of some simple list functions, including car, cadr, and append.

An example session using this LISP REPL is shown below
$ mono lisp.exe
PLD LISP v. 1.0 
> (load listfunctions) 
= () 
> (append ’(a b c) ’(d e f)) 
= (a b c d e f) 
>(+ (-3(-2)
