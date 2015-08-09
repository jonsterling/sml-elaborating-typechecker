functor Refiner (Evaluator : EVALUATOR where type term = EvidenceAbt.t) : REFINER =
struct
  structure Lcf = Lcf
  structure Tacticals = Tacticals (Lcf)

  structure Sequent = Sequent
  structure Verification = EvidenceAbt

  open Sequent
  infix >>

  exception Evidence of string
  exception Refine of string

  open Prop Evidence
  structure P = PropAbt and E = EvidenceAbt and T = Telescope

  type name = EvidenceAbt.Variable.t
  type rule = Lcf.tactic

  structure Rules =
  struct
    fun Assumption z (H >> P) =
      let
        val P' = T.lookup H z
        val _ = if PropAbt.eq (P, P') then () else raise Refine "Assumption"
      in
        ([], fn [] => E.`` z
              | _ => raise Evidence "Assumption")
      end

    fun ImpliesRight x (H >> implication) =
      let
        val P.$ (IMPLIES, #[P, Q]) = P.out implication
        val H' = T.snoc H (x, P)
      in
        ([H' >> Q], fn [D] => E.$$ (LAM, #[E.\\ (x, D)])
                     | _ => raise Evidence "ImpliesRight")
      end

    fun ImpliesLeft z x (goal as H >> C) =
      let
        val P.$ (IMP, #[P, Q]) = P.out (T.lookup H z)
        val H' = T.interposeAfter H (z, T.snoc T.empty (x, Q))
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
        val P.$ (AND, #[P,Q]) = P.out (T.lookup H z)
        val H' = T.interposeAfter H (z, T.snoc (T.snoc T.empty (s, P)) (t, Q))
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
        val P.$ (OR, #[P, Q]) = P.out (T.lookup H z)
        val H1 = T.interposeAfter H (z, T.snoc T.empty (x, P))
        val H2 = T.interposeAfter H (z, T.snoc T.empty (y, Q))
      in
        ([H1 >> C, H2 >> C], fn [D,E] => E.$$ (DECIDE, #[E.`` z, E.\\ (x, D), E.\\ (y, E)])
                              | _ => raise Evidence "OrLeft")
      end
  end

  structure AdmissibleRules =
  struct
    fun Cut (x, P) (H >> C) =
      let
        val H' = T.snoc H (x, P)
      in
        ([H >> P, H' >> C], fn [D, E] => Evaluator.evalOpen (E.subst D x E)
                             | _ => raise Evidence "Cut")
      end
  end
end

structure Refiner = Refiner (Evaluator)
