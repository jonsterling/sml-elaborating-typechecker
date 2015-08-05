signature REFINER =
sig
  structure Tacticals : TACTICALS
     where type tactic = Lcf.tactic
       and type goal = Lcf.goal

  exception Refine of string
  exception Evidence of string

  structure Rules :
  sig
    val Assumption : Var.t -> Lcf.tactic
    val ImpliesRight : Var.t -> Lcf.tactic
    val ImpliesLeft : Var.t -> Var.t -> Lcf.tactic
    val OrRight1 : Lcf.tactic
    val OrRight2 : Lcf.tactic
    val OrLeft : Var.t -> Var.t * Var.t -> Lcf.tactic
    val TrueRight : Lcf.tactic
    val AndRight : Lcf.tactic
    val AndLeft : Var.t -> Var.t * Var.t -> Lcf.tactic
  end

  structure AdmissibleRules :
  sig
    val Cut : Var.t * Judgment.prop -> Lcf.tactic
  end
end

