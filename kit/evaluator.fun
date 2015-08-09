functor SmallStepEvaluator
  (structure Abt : ABT
   structure SmallStep : SMALL_STEP
     where type term = Abt.t) : EVALUATOR =
struct
  type term = Abt.t

  structure SmallStepUtil = SmallStepUtil (SmallStep)

  fun eval E = #1 (SmallStepUtil.steps (E, NONE))
  fun evalOpen E = eval E handle SmallStep.Stuck E => E
end
