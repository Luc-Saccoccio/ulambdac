# ULambdaC

The Untyped Lambda Calculus REPL

## Grammar

```
expr ::= application | term
term ::= alias | lambdaAbs | var | '(' application ')'
alias ::= ALIASIDENTIFIER ":=" expr
lambdaAbs ::= ('\' | 'λ') VARIABLEIDENTIFIER '.' application
var ::= ALIASIDENTIFIER | VARIABLEIDENTIFIER
application ::= term term+

ALIASIDENTIFIER ::= _any single upper case letter_ | _any single digit_
VARIABLEIDENTIFIER ::= _any single lower case letter_
```

## Usage notes

### Barendregt's convention

This implementation follows Barendregt's convention, that is :

> "We choose the names for the binding variables in a λ-term in such a
> manner that they are all different and such that each of them differs
> from all free variables occurring in the term."

[1]

### Auto-reduction

Auto-reduction has no failsafe for checking programs that are not reducible to normal form. Plus, the strategy used is _probably_ applicative order (unsure because this is a toy program and I don't know what I'm doing anyway). The [Church-Rosser theorem](https://en.wikipedia.org/wiki/Church%E2%80%93Rosser_theorem) assures that we obtain what we want anyway :D

### Aliases

The alias `M := f` is defined as semantically equivalent to `(λx.x)` with the added effect of defining `M` to be equal to `f` in the current application :

```
(M := (λx.xx))(MM)
=> (λx.xx)(λx.xx)
```

If used in the global context (i.e. not as an application), it'll be added to the global context of the REPL :

```
M := (λx.xx)
MM
=> (λx.xx)(λx.xx)
```

It is possible to list bindings with `:b` and delete one with `:d M` or `:delete M` where `M` is an alias identifier. Deleting a non-existing identifier has no effect on the global context.

### Recursion and mutual recursion

Since aliases are stored syntactically, they do not reference the currently stored aliases. Concretely :
```
λ> M := Iy
λ> I := λx.x
λ> #ar M
y
λ> I := \x.xx
λ> #ar M
yy
```

This has the (totally planned) effect of allowing recursion and mutual recursion between lambda terms :D !

## TODO

- [x] Subterms
- [x] Free Variables
- [x] Substitution (through aliases)
- [x] Automatic reduction
- [ ] Redexes
- [ ] Manual reduction
- [x] REPL history

## References

[1]: "Type Theory and Formal Proof, An Introduction", Rob Nederpelt & Herman Geuvers, ISBN 978-1-107-03650-5
