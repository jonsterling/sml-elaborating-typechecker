signature SMALL_STEP =
sig
  type term

  datatype t = STEP of term | CANON
  exception Stuck of term

  val step : term -> t
end

signature SMALL_STEP_UTIL =
sig
  include SMALL_STEP

  type petrol = int

  (* Evaluate a term with a bit of petrol, returning the result and the amount
   * of petrol expended. *)
  val steps : term * petrol option -> term * petrol
end
