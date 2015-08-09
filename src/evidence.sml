structure Evidence =
struct
  datatype t = AX | LAM | AP | INL | INR | DECIDE | FST | SND | PAIR

  val eq : t * t -> bool = op=

  fun arity AX = #[]
    | arity LAM = #[1]
    | arity AP = #[0,0]
    | arity INL = #[0]
    | arity INR = #[0]
    | arity DECIDE = #[0,1,1]
    | arity PAIR = #[0,0]
    | arity FST = #[0]
    | arity SND = #[0]

  fun toString LAM = "λ"
    | toString AP = "ap"
    | toString INL = "inl"
    | toString INR = "inr"
    | toString DECIDE = "decide"
    | toString AX = "<>"
    | toString PAIR = "pair"
    | toString FST = "fst"
    | toString SND = "snd"
end

structure EvidenceAbt = AbtUtil (Abt (structure Variable = Variable and Operator = Evidence))
