functor MkLcf (type judgment and evidence) : LCF =
struct
  type goal = judgment
  type evidence = evidence
  type validation = evidence list -> evidence
  type tactic = goal -> goal list * validation
end

functor Lcf (J : JUDGMENT) : LCF =
  MkLcf (type judgment = J.t and evidence = EvidenceAbt.t)

structure Lcf = Lcf (Judgment)

