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

This implementation follows Barendregt's convention, that is :

> "We choose the names for the binding variables in a λ-term in such a
> manner that they are all different and such that each of them differs
> from all free variables occurring in the term."

[1]

## TODO

- [x] Subterms
- [ ] Free Variables
- [ ] Redexes
- [ ] Substitution
- [ ] β-reduction
- [ ] Automatic reduction
- [ ] Manual reduction
- [ ] REPL history

## References

[1]: "Type Theory and Formal Proof, An Introduction", Rob Nederpelt & Herman Geuvers, ISBN 978-1-107-03650-5
