functor Refiner (Evaluator : EVALUATOR) : REFINER =
struct
  structure Lcf = Lcf
  structure Tacticals = Tacticals (Lcf)

  open Judgment
  infix >>

  exception Evidence of string
  exception Refine of string

  open Prop Evidence
  structure P = PropAbt and E = EvidenceAbt

  structure Rules =
  struct
    fun Assumption z (H >> P) =
      let
        val P' = Telescope.lookup H z
        val _ = if PropAbt.eq (P, P') then () else raise Refine "Assumption"
      in
        ([], fn [] => E.`` z
             | _ => raise Evidence "Assumption")
      end

    fun ImpliesRight x (H >> implication) =
      let
        val P.$ (IMPLIES, #[P, Q]) = P.out implication
        val x = Telescope.fresh (H, x)
        val H' = Telescope.snoc H (x, P)
      in
        ([H' >> Q], fn [D] => E.$$ (LAM, #[E.\\ (x, D)])
                    | _ => raise Evidence "ImpliesRight")
      end

    fun ImpliesLeft z x (goal as H >> C) =
      let
        val P.$ (IMP, #[P, Q]) = P.out (Telescope.lookup H z)
        val x = Telescope.fresh (H, x)
        val H' = Telescope.interposeAfter H (z, Telescope.snoc Telescope.empty (x, Q))
      in
        ([H >> P, H' >> C],
         fn [D,E] => E.subst (E.$$ (AP, #[E.``z, D])) x E
         | _ => raise Evidence "ImpliesLeft")
      end

    fun TrueRight (H >> C) =
      let
        val P.$ (TRUE, #[]) = P.out C
      in
        ([], fn [] => E.$$ (AX, #[])
             | _ => raise Evidence "TrueRight")
      end

    fun AndRight (H >> C) =
      let
        val P.$ (AND, #[P,Q]) = P.out C
      in
        ([H >> P, H >> Q], fn [D,E] => E.$$ (PAIR, #[D,E])
                           | _ => raise Evidence "AndRight")
      end

    fun AndLeft z (s,t) (H >> C) =
      let
        val P.$ (AND, #[P,Q]) = P.out (Telescope.lookup H z)
        val s = Telescope.fresh (H, s)
        val t = Telescope.fresh (H, t)
        val H' = Telescope.interposeAfter H (z, Telescope.snoc (Telescope.snoc Telescope.empty (s, P)) (t, Q))
      in
        ([H' >> C], fn [D] => E.subst (E.$$ (FST, #[E.``z])) s (E.subst (E.$$ (SND, #[E.``z])) t D)
                    | _ => raise Evidence "AndLeft")
      end

    fun OrRight1 (H >> C) =
      let
        val P.$ (OR, #[P, Q]) = P.out C
      in
        ([H >> P], fn [D] => E.$$ (INL, #[D])
                   | _ => raise Evidence "OrRight1")
      end

    fun OrRight2 (H >> C) =
      let
        val P.$ (OR, #[P, Q]) = P.out C
      in
        ([H >> Q], fn [D] => E.$$ (INR, #[D])
                 | _ => raise Evidence "OrRight2")
      end

    fun OrLeft z (x, y) (H >> C) =
      let
        val P.$ (OR, #[P, Q]) = P.out (Telescope.lookup H z)
        val H1 = Telescope.interposeAfter H (z, Telescope.snoc Telescope.empty (x, P))
        val H2 = Telescope.interposeAfter H (z, Telescope.snoc Telescope.empty (y, Q))
      in
        ([H1 >> C, H2 >> C], fn [D,E] => E.$$ (DECIDE, #[E.`` z, E.\\ (x, D), E.\\ (y, E)])
                             | _ => raise Evidence "OrLeft")
      end
  end

  structure AdmissibleRules =
  struct
    fun Cut (x, P) (H >> C) =
      let
        val H' = Telescope.snoc H (x, P)
      in
        ([H >> P, H' >> C], fn [D, E] => Evaluator.evalOpen (E.subst D x E)
                          | _ => raise Evidence "Cut")
      end
  end
end

structure Refiner = Refiner (Evaluator)
