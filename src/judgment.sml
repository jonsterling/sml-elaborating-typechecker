structure Judgment :
sig
  include JUDGMENT
  structure Telescope : TELESCOPE
end =
struct
  structure Telescope = Telescope (Var)
  type prop = PropAbt.t

  type context = prop Telescope.telescope
  datatype t = >> of context * prop
  infix >>
  fun toString (H >> C) =
    Telescope.toString PropAbt.toString H ^ " >> " ^ PropAbt.toString C
end
