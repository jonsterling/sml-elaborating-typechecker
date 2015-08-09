signature SEQUENT =
sig
  type prop

  structure Telescope : TELESCOPE
  type context = prop Telescope.telescope
  type name = Telescope.label

  datatype t = >> of context * prop
  val toString : t -> string
end

