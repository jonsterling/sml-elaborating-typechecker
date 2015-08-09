functor Typechecker
  (structure Sequent : SEQUENT
   structure Lcf : LCF
     where type goal = Sequent.t
   structure Elaborator : ELABORATOR
     where type tactic = Lcf.tactic) : TYPECHECKER =
struct
  type prop = Sequent.prop
  type term = Elaborator.term
  type verification = Lcf.evidence

  structure Tacticals = Tacticals (Lcf)
  open Tacticals Sequent
  infix >>

  fun check (M, A) =
    let
      val script = Elaborator.elab M
      val (subgoals, validation) =
        TRY script (Telescope.empty >> A)
    in
      if length subgoals > 0 then
        (print
          ("\nRemaining subgoals:\n\n" ^
           foldl (fn (g,r) => r ^ "\n" ^ Sequent.toString g ^ "\n") "" subgoals ^ "\n");
         raise Fail "Refinement failed")
      else
        validation []
    end
end
