structure Sequent : SEQUENT =
struct
  structure Telescope = Telescope (Variable)
  type prop = PropAbt.t
  type name = Telescope.label

  type context = prop Telescope.telescope
  datatype t = >> of context * prop
  infix >>
  fun toString (H >> C) =
    Telescope.toString PropAbt.toString H ^ " >> " ^ PropAbt.toString C
end
