functor SmallStepUtil (S : SMALL_STEP) : SMALL_STEP_UTIL =
struct
  open S
  type petrol = int

  local
    val guzzle = Option.map (fn x => x - 1)
    fun expended (SOME x) = x <= 0
      | expended NONE = false

    fun go (M, gas) i =
      if expended gas then
        (M,i)
      else
        case step M of
            STEP M' => go (M', guzzle gas) (i + 1)
          | CANON => (M,i)
  in
    fun steps (M, gas) = go (M, gas) 0
  end
end

