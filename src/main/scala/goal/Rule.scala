package goal

/**
  * Rule
  *
  * A rule to be applied to a state, if it matches it transforms the state.
  */
class Rule[S <: State](fn: PartialFunction[S, Action[S, S]]) extends PartialFunction[S, Action[S, S]] {

  type In = S
  type Out = Action[S, S]

  override def isDefinedAt(x: In): Boolean = fn.isDefinedAt(x)

  override def apply(v1: In): Out = fn.apply(v1)

  def orElse[In, Out](that: Rule[S]): Rule[S] = new Rule(super.orElse(that))
}

/**
  * Named Rule
  *
  * A rule to be applied to a state, if it matches it transforms the state.
  */
case class NamedRule[S <: State](name: String, fn: PartialFunction[S, Action[S, S]]) extends Rule(fn)

/**
  * Compound Rule
  *
  * Supplied rule order is relevant.
  */
case class CompoundRule[S <: State](rules: Rule[S]*) extends Rule(rules.reduce { _ orElse _ })