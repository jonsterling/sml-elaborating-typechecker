signature EVALUATOR =
sig
  type term
  val eval : term -> term
  val evalOpen : term -> term
end
