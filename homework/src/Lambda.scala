

/**
 * Created by Шолохов on 27.05.2015.
 */
case class Lambda(v: Variable, body: Term) extends Term {
  override def toString: String = "\\" + v + "." + "(" + body + ")"

  override def getFreeVariables(vars: Set[Variable]): Set[Variable] = body.getFreeVariables(vars)- v

  override def renameVariable(v: Variable): Unit = {this.v.renameVariable(v); body.renameVariable(v)}

  override def getChainedVariables(vars: Set[Variable]): Set[Variable] = vars + v

  override def getAllVariables(vars: Set[Variable]): Set[Variable] = vars + v
}
