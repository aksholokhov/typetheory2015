

/**
 * Created by Шолохов on 27.05.2015.
 */
case class Application(a: Term, b: Term) extends Term{
  override def toString: String = "(" + a + ")" + " " + "(" + b + ")"

  override def getFreeVariables(vars: Set[Variable]): Set[Variable] = vars ++ a.getFreeVariables(vars) ++ b.getFreeVariables(vars)

  override def renameVariable(v: Variable): Unit = {a.renameVariable(v) ; b.renameVariable(v)}

  override def getChainedVariables(vars: Set[Variable]): Set[Variable] = a.getChainedVariables(vars) ++ b.getChainedVariables(vars)

  override def getAllVariables(vars: Set[Variable]): Set[Variable] = a.getAllVariables(vars) ++ b.getAllVariables(vars)
}
