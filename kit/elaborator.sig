signature ELABORATOR =
sig
  type term
  type tactic
  val elab : term -> tactic
end
