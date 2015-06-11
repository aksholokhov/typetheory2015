package sholokhov.lambdas.parser

/**
 * Created by Шолохов on 30.05.2015.
 */
case class Condition(where: Term, v: Variable, what: Term) extends Term {
  override def getFreeVariables(vars: Set[Variable]): Set[Variable] = null
  override def renameVariable(v: Variable): Unit = {}

  override def getChainedVariables(vars: Set[Variable]): Set[Variable] = ???
}