structure Evaluator : EVALUATOR =
struct
  open Evidence EvidenceAbt
  infix $ \ $$ \\ //

  structure SmallStepUtil = SmallStepUtil (SmallStep)

  fun eval E = #1 (SmallStepUtil.steps (E, NONE))
  fun evalOpen E = eval E handle SmallStep.Stuck E => E
end
