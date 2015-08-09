signature REFINER =
sig
  structure Sequent : SEQUENT
  structure Verification : ABT_UTIL

  structure Lcf : LCF
    where type goal = Sequent.t
    where type evidence = Verification.t

  type name = Sequent.name
  type rule = Lcf.tactic

  exception Refine of string
  exception Evidence of string

  structure Rules :
  sig
    val Assumption : name -> rule
    val ImpliesRight : name -> rule
    val ImpliesLeft : name -> name -> rule
    val OrRight1 : rule
    val OrRight2 : rule
    val OrLeft : name -> name * name -> rule
    val TrueRight : rule
    val AndRight : rule
    val AndLeft : name -> name * name -> rule
  end

  structure AdmissibleRules :
  sig
    val Cut : name * Sequent.prop -> rule
  end
end

