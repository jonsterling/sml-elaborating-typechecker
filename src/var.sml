structure Var =
struct
  structure V = Variable ()
  open V

  val counter = ref 0
  fun named x =
    let
      val i = !counter
    in
      counter := i + 1;
      V.named (x ^ Int.toString i)
    end
end


