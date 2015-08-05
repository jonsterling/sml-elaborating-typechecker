signature ELABORATOR =
sig
  structure Lcf : LCF
  type term
  val elab : term -> Lcf.tactic
end
