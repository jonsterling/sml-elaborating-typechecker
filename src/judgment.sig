signature JUDGMENT =
sig
  type prop
  type context

  datatype t = >> of context * prop
  val toString : t -> string
end

