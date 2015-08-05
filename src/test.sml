structure Test =
struct
  open Elaborator Judgment Evidence Prop PropAbt Refiner
  open Rules Tacticals
  infix THEN THENL ORELSE

  infix 2 >>
  infix 9 $$ \\

  local
    open EvidenceAbt infix 9 $$ infix 8 \\
    fun lam name k =
      let
        val x = Variable.named name
      in
        LAM $$ #[x \\ k x]
      end
    val identity = lam "x" ``
    fun inl M = INL $$ #[M]
    val ax = AX $$ #[]
    val y = Variable.named "y"
    val z = Variable.named "z"
  in
    val term = lam "p" (fn p => DECIDE $$ #[AP $$ #[``p, ax], (y \\ DECIDE $$ #[``y, z \\ ``z, z \\ ``z]), (y \\ ``y)])
  end

  local
    open PropAbt infix 9 $$ infix 8 \\
    val True = TRUE $$ #[]
    fun *> (p, q) = IMP $$ #[p, q]
    infixr *>
    fun Disj p q = OR $$ #[p, q]
    fun Conj p q = AND $$ #[p, q]
  in
    val prop = (True *> Disj (Disj True True) True) *> True
  end

  val script = Elaborator.elab term
  val ([], validation) =
      COMPLETE script (Telescope.empty >> prop)
      handle Refine msg => raise Fail ("Refine: " ^ msg)
           | Evidence msg => raise Fail ("Evidence: " ^ msg)
           | EvidenceAbt.Malformed msg => raise Fail msg
           | RemainingSubgoals goals =>
             (print "\n";
              app (fn j => print (Judgment.toString j ^ "\n\n")) goals;
              raise Fail "Remaining subgoals")
  val evidence = validation []
  val _ = print (EvidenceAbt.toString term ^ " ▹ " ^ EvidenceAbt.toString evidence ^ " : " ^ PropAbt.toString prop ^ "\n")
end
