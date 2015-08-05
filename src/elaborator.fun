functor Elaborator (structure Refiner : REFINER and Evaluator : EVALUATOR) : ELABORATOR =
struct
  structure P = PropAbt and E = EvidenceAbt
  open Refiner Prop Evidence Judgment
  open Tacticals Rules AdmissibleRules Evaluator
  infix THEN THENL ORELSE
  open E infix $ \ $$ \\ // >>

  fun FAIL' tag M _ =
    raise Fail (tag ^ ": " ^ E.toString M)

  fun trace tag M =
    TRACE (tag ^ ": " ^ E.toString M)

  fun elab M =
    let
      val M = evalOpen M
    in
      elabByTerm M ORELSE elabByType M
    end

  and elabByTerm M =
      case out M of
          `x => Assumption x
        | AX $ #[] => TrueRight
        | AP $ #[R,N] =>
          let
            val z = Var.named "z"
            val x = Var.named "x"
          in
            elimRule (z, R) THEN ImpliesLeft z x THENL [elab N, Assumption x]
          end
        | FST $ #[R] =>
          let
            val z = Var.named "z"
            val s = Var.named "s"
            val t = Var.named "t"
          in
            elimRule (z, R) THEN AndLeft z (s, t) THEN Assumption s
          end
        | SND $ #[R] =>
          let
            val z = Var.named "z"
            val s = Var.named "s"
            val t = Var.named "t"
          in
            elimRule (z, R) THEN AndLeft z (s, t) THEN Assumption t
          end
        | DECIDE $ #[R, xE, yF] =>
          let
            val z = Var.named "z"
            val (x, E) = unbind xE
            val (y, F) = unbind yF
          in
            elimRule (z, R) THEN OrLeft z (x, y) THENL [elab E, elab F]
          end
        | _ => FAIL' "elabByTerm" M

  and elimRule (z, R) (goal as H >> P) =
      let
        val A = synthesizeType (H, R)
      in
        (Cut (z, A) THENL [elab R, ID]) goal
      end

  and elabByType M (goal as H >> P) =
      (case P.out P of
           P.$ (IMP, #[A,B]) =>
           let
             val x = Var.named "ξ"
             val Ex = AP $$ #[M, ``x]
           in
             ImpliesRight x THEN elab Ex
           end
         | _ => FAIL' "elabByType" M
      ) goal

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
        | DECIDE $ #[R, xE, yF] =>
          let
            val P.$ (OR, #[P,Q]) = P.out (synthesizeType (H, R))
          in
            raise Fail ""
          end
        | _ => raise Fail "Cannot synthesize type"

end

structure Elaborator = Elaborator (structure Refiner = Refiner and Evaluator = Evaluator)
