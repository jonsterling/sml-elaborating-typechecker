signature TYPECHECKER =
sig
  type prop
  type term
  type proof

  val check : term * prop -> proof
end
