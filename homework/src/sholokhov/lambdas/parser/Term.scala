package sholokhov.lambdas.parser

/**
 * Created by Шолохов on 27.05.2015.
 */
trait Term {
  def getFreeVariables(vars: Set[Variable]): Set[Variable]
  def getChainedVariables(vars: Set[Variable]): Set[Variable]
  def getAllVariables(vars: Set[Variable]): Set[Variable]
  def renameVariable(v: Variable): Unit
}
