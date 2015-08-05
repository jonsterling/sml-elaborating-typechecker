A proposition `P` is defined by its intention (i.e. what counts as a
verification of that proposition); to know `P prop` is to have understood this
intention, and to know `P ver` is to have fulfilled it (by constructing a
verification object).  To know `P true` is to have a plan to verify `P`.

We begin with a LCF-with-validations refiner; ephemerally, inferences in this
refiner are derivations in the sequent calculus with cut, but the synthesis of
the refiner is a cut-free verification object. (In LCF-with-validations, you
have both derivable rules and admissible rules; Cut is an admissible rule.)

Then, we make precise what was meant by "a plan to verify `P`"; in this case,
we intend it to mean an arbitrary lambda term which may have redexes. An
elaborator is then defined which translates such lambda terms into tactic
scripts in the sequent calculus; then, these scripts synthesize a normal
verification object in case the plan to verify `P` succeeded.

To summarize:

1. Translate a natural deduction proof (lambda term) into an (ephemeral)
   derivation in the sequent calculus with cut.

2. Run this through the refiner, which synthesizes a verification object with
   all cuts eliminated.

This technique is analogous to normalization-by-hereditary-substitution, since
hereditary substitutions are the constructive content of the admissibility of
cut.
