signature TYPECHECKER =
sig
  type prop
  type term
  type verification

  val check : term * prop -> verification
end
