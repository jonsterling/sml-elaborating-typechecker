structure Test =
struct
  open Evidence Prop PropAbt Typechecker

  infix 9 $$ \\

  local
    open EvidenceAbt infix 9 $$ infix 8 \\
    fun lam name k =
      let
        val x = Variable.named name
      in
        LAM $$ #[x \\ k x]
      end
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

  val proof = Typechecker.check (term, prop)
  val _ = print (EvidenceAbt.toString term ^ " ▹ " ^ Refiner.Verification.toString proof ^ " : " ^ PropAbt.toString prop ^ "\n")
end
