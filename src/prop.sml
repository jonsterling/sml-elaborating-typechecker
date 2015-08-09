structure Prop =
struct
  datatype t = IMP | TRUE | OR | AND

  fun arity IMP = #[0,0]
    | arity OR = #[0,0]
    | arity AND = #[0,0]
    | arity TRUE = #[]

  val eq : t * t -> bool = op=

  fun toString IMP = "=>"
    | toString TRUE = "true"
    | toString AND = "and"
    | toString OR = "or"
end

structure PropAbt = AbtUtil (Abt (structure Variable = Variable and Operator = Prop))
