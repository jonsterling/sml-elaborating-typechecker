functor Elaborator
  (structure Refiner : REFINER
     where type Sequent.Telescope.Label.t = EvidenceAbt.Variable.t
     where type Sequent.prop = PropAbt.t
   structure Evaluator : EVALUATOR
     where type term = EvidenceAbt.t) : ELABORATOR =
struct
  structure P = PropAbt and E = EvidenceAbt
  open Refiner Prop Evidence
  open Rules AdmissibleRules Evaluator Sequent
  infix THEN THENL ORELSE
  open E infix $ \ $$ \\ // >>

  type term = EvidenceAbt.t
  type tactic = Lcf.tactic

  structure Tacticals = Tacticals (Refiner.Lcf)
  open Tacticals

  fun elab M = elab' (evalOpen M)

  and elab' M =
      case out M of
          `x => Assumption x
        | AX $ #[] => TrueRight
        | LAM $ #[xE] =>
          let
            val (x, E) = unbind xE
          in
            ImpliesRight x THEN elab E
          end
        | INL $ #[M] => OrRight1 THEN elab M
        | INR $ #[M] => OrRight2 THEN elab M
        | PAIR $ #[M,N] => AndRight THENL [elab M, elab N]
        | AP $ #[R,N] =>
          let
            val z = Variable.named "z"
            val x = Variable.named "x"
          in
            elimRule (z, R) THEN ImpliesLeft z x THENL [elab N, Assumption x]
          end
        | FST $ #[R] =>
          let
            val z = Variable.named "z"
            val s = Variable.named "s"
            val t = Variable.named "t"
          in
            elimRule (z, R) THEN AndLeft z (s, t) THEN Assumption s
          end
        | SND $ #[R] =>
          let
            val z = Variable.named "z"
            val s = Variable.named "s"
            val t = Variable.named "t"
          in
            elimRule (z, R) THEN AndLeft z (s, t) THEN Assumption t
          end
        | DECIDE $ #[R, xE, yF] =>
          let
            val z = Variable.named "z"
            val (x, E) = unbind xE
            val (y, F) = unbind yF
          in
            elimRule (z, R) THEN OrLeft z (x, y) THENL [elab E, elab F]
          end
        | _ => FAIL

  and elimRule (z, R) (goal as H >> P) =
      let
        val A = synthesizeType (H, R)
      in
        (Cut (z, A) THENL [elab R, ID]) goal
      end

  and synthesizeType (H, M) =
      case out (evalOpen M) of
          `x => Telescope.lookup H x
        | AP $ #[R,N] =>
          let
            val P.$ (IMP, #[P,Q]) = P.out (synthesizeType (H, R))
          in
            Q
          end
        | FST $ #[R] =>
          let
            val P.$ (AND, #[P,Q]) = P.out (synthesizeType (H, R))
          in
            P
          end
        | SND $ #[R] =>
          let
            val P.$ (AND, #[P,Q]) = P.out (synthesizeType (H, R))
          in
            Q
          end
        | _ => raise Fail "Cannot synthesize type"

end

structure Elaborator = Elaborator (structure Refiner = Refiner and Evaluator = Evaluator)
