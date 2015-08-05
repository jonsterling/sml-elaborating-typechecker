functor Typechecker
  (structure Refiner : REFINER
   structure Elaborator : ELABORATOR
     where type Lcf.goal = Refiner.Lcf.goal
     where type Lcf.evidence = Refiner.Lcf.evidence) : TYPECHECKER =
struct
  type prop = PropAbt.t
  type term = Elaborator.term
  type proof = Refiner.Lcf.evidence

  structure Tacticals = Tacticals (Refiner.Lcf)
  open Tacticals Judgment
  infix >>

  fun check (M, A) =
    let
      val script = Elaborator.elab M
      val ([], validation) = COMPLETE script (Telescope.empty >> A)
    in
      validation []
    end
end

structure Typechecker = Typechecker (structure Elaborator = Elaborator and Refiner = Refiner)
