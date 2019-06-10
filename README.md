# lambda

#### A toy interpreter and compiler of the untyped λ calculus, written in common lisp.

A toy compiler of the untyped lambda calculus into common lisp (so far), written in common lisp, with an interpreter and a user interface (UI).

The UI includes two REPLs and a viewer of all available transformations of a user-input λ term.

##### Goal

The goal of this project is to compile the untyped λ calculus and extensions thereof into several targets, such as a stack machine, and some assembly languages.

For learning and understanding compilers, therefore I write most of it by hand. Written in common lisp because I am most comfortable with this language.

## Try it!

Requirements

- Common Lisp REPL (SBCL and CCL tested) with Quicklisp installed

Clone this repository to a directory where Quicklisp can find it, for example in `quicklisp/local-projects/` or in whatever `(:tree ...)` you have in `~/.config/common-lisp/repos.conf`.

In your Common Lisp REPL evaluate:
   
```
(ql:quickload "lambda")
(in-package :lambda.ui)
(main)
```
   
You'll be prompted: 

```
Input which UI to use:
  3  for the transformation viewer
  2  for the church REPL
  1  for the base REPL
  0  to exit
Anything else will return help instructions.
```

The UI help instructions include the BNF of the standard syntax of this λ calculus, and a short description of each menu options. Each UI mode starts with help instructions specific to the UI mode, examples of usage, and the set of self encoded primitives, such as `:ID :TRUE :FALSE :S :AND :OR :IF :Y :OMEGA`, which you can use when entering a λ term.

#### Examples of λ terms, without and with primitives, and of reductions.

Note that only single variable abstractions are allowed.

Without primitives:

```
d
(x y)
(λ a (λ b (a b))
```

With primitives:

```
((:or :true) a)
(((:if :false) a ) b)
```

In the UI's "base REPL", the above evaluate to `:true` and `b`, respectively.

## Implementation features
  - A User Interface (UI) in the package `:lambda.ui` (call `(main)`) using self-encoded primitives, which include:
      + Two Read Eval Print Loops (REPLs) for this λ calculus:
          * `λ.base` allows you to use the following symbols as primitives: `:ID :TRUE :FALSE :S :AND :OR :IF :Y :OMEGA`
          * `λ.church` allows you to use base, as well as numbers in your statements, and the following symbols as primitives for numeric functions:
            `:A+ :A* :Aexp :Azero? :A+1 :A-1 :A-factorial`, for plus, times, exponent, zero test, successor, predecessor, and factorial.
      + A transformation viewer: Input a λ term and see its available representations (see next list item).
  - Showcases representations of lambda terms in my quest to understand compilers.
    So far, showcased representations of lambda terms are the following.
      + The standard, single variable binding, parenthesised lambda terms as quoted common-lisp lists, for example, `'(^ x (x y))`.
      + De Bruijn representation (locally nameless for open terms), the example becomes `'(^ (0 y))`.
      + A common lisp representation, the example becomes `'(lambda (x) (funcall x y))`.
      + TBA: Implementation of Tromp's
        * combinatorial logic representation in the form of SKI-calculus (towards a stack machine),
        * and his binary representation.
      + TBA: Implementation of Mogensen's continuation passing style self encoding of lambda calculus (Mogensen-Scott encoding).
      + I provide transformations between defined representations.
  - Lazy, normal order, and applicative order reducers, with respective steppers and self-evaluation loop catchers.
  - A λ calculus interpreter in common-lisp and a compiler to common lisp lambda abstractions.
  - Compiler transformations and simplifications are currently only β-reducing in normal order with loop-catching and maximum allowed steps, closing possibly open terms, full renaming of bound variables, then switching to common-lisp syntax and returning the resulting common-lisp closure.
  
  For the base of this implementation I just followed Barendrecht and Barendsen's 1994 book "Introduction to Lambda Calculus" [BarBar84]. TBA representations come with respective papers (see references below), which I am currently implementing.

#### Tested
   
   By adding a catch-loop stepper-based reducer of the standard representation, I have been able to use cl-quickcheck on arbitrary generated terms, and
   quickcheck also examples, theorems, and lemmas from [BarBar84].
   
   Run the quickcheck tests by evaluating `(lambda.quickcheck:run-quickcheck)`.
   
   Extensive manually written tests are performed as well by evaluating: `(lambda.test:test-all)`
   
#### Documented

I used the principle of literal programming, with comments in plain text format. The beginning of each file should be informative of the file contents.
   

# TODO

Immediate TODO goals, in my priority order, starting from highest priority.
-  Implement Tromp's Combinatorial Logic representation in the form of SKI-calculus (towards a stack machine), from his 2018 paper [Tro18].
-  Find a nicer way around printing keywords with their dots (print related functions in ui.lisp are messy!)
-  Implement UI for steppers.
-  Find a faster encoding for natural numbers or integers, or/and get common lisp (or other target languages, such as the above mentioned stack machine)
-  Implement Tromp's binary format for closed terms, again from [Tro18].
-  Compile to my computer's assembly (with the help of disassembly).
-  Implement more encodings or/and replace them with primitives of a target language.
-  Unify the 3 UI "modes" into one big REPL. Low priority because it's probably not helping in the main goals of this project.

# References
 
In several places in documentation and in comments, I refer to the following publications, in LaTeX bibtex-style alpha.

- [Bar84] **The Lambda Calculus: Its Syntax and Semantics**, H.P. Barendregt, *Elsevier (1984)*
- [BarBar84] **Introduction to lambda calculus**, H.P. Barendregt and E.Barendsen, *Nieuw archief voor wisenkunde 4, 337-372 (1984)*
- [Tro18] **Functional Bits : Lambda Calculus based Algorithmic Information Theory**, J. Tromp, *<https://tromp.github.io/cl/LC.pdf> (2018)*
- [Mog94] **Efficient Self-Interpretation in Lambda Calculus**, T. Mogensen, *Journal of Functional Programming 2, (1994)*

