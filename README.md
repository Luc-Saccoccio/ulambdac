# ULambdaC

The Untyped Lambda Calculus REPL (Acronym In Progress)

## Grammar

```
expr ::= application | term
term ::= alias | lambdaAbs | var | '(' application ')'
alias ::= ALIASIDENTIFIER ":=" expr
lambdaAbs ::= ('\' | 'λ') VARIABLEIDENTIFIER '.' application
var ::= ALIASIDENTIFIER | VARIABLEIDENTIFIER
application ::= term term+

ALIASIDENTIFIER ::= _any single upper case letter_
VARIABLEIDENTIFIER ::= _any single lower case letter_
```

## Usage notes

## Barendregt's convention

This implementation follows Barendregt's convention, that is :

> "We choose the names for the binding variables in a λ-term in such a
> manner that they are all different and such that each of them differs
> from all free variables occurring in the term."

[1]


## Aliases

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

## TODO

- [x] Subterms
- [x] Free Variables
- [ ] Redexes
- [ ] Substitution
- [ ] β-reduction
- [ ] Automatic reduction
- [ ] Manual reduction
- [ ] REPL history

## References

[1]: "Type Theory and Formal Proof, An Introduction", Rob Nederpelt & Herman Geuvers, ISBN 978-1-107-03650-5
