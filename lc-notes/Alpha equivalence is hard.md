Before any two expressions are checked for $\alpha$-equivalence, there is a replacement step that replaces every unbound identifier with the appropriate definition in the loaded *program* file. This step was implemented in a way, that violated the [[Expression-Identifier Invariance Rule]].

The problem was that the replacement step didn't take into account, that the replaced expressions could be $\beta$-reduced after replacement. When an identifier was replaced with the corresponding definition from the program file, the replacement expression had exactly the same identifier numbers as the expression defined in the program file. This created situations where the EIIR could be violated.

## Example

Consider the following definitions. (Identifier numbers are added for better understanding):
```
zero_1 := \f_2.\z_3.z_3
suc_4 := \n_5.\f_6.\z_7. f_6 (n_5 f_6 z_7)
one_8 := suc_4 zero_1
```

Doing the replacement step on the expression `one_8 suc_4` would look like this:
1. `one_8 suc_4`
2. `(suc_4 zero_1) suc_4`
3. `((\n_5.\f_6.\z_7. f_6 (n_5 f_6 z_7)) zero_1) suc_4`
4. `((\n_5.\f_6.\z_7. f_6 (n_5 f_6 z_7)) (\f_2.\z_3.z_3)) suc_4`
5. `((\n_5.\f_6.\z_7. f_6 (n_5 f_6 z_7)) (\f_2.\z_3.z_3)) (\n_5.\f_6.\z_7. f_6 (n_5 f_6 z_7))`

Doing a couple $\beta$-reductions on this will result in the following expression:
```
\z_7.\f_6.\z_7.f_6 (z_7 f_6 z_7)
```
The EIIR has been violated.

## Solution

The solution was to insert a duplication step before replacing an identifier. This duplication step would create an $\alpha$-equivalent expression, whose identifier numbers don't appear anywhere else in the code.

> **Note:** The duplication step does leave references to the program file untouched. This happens because with broken references $\alpha$-equivalence cannot be guaranteed.

