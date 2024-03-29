

/**
 * Created by Шолохов on 27.05.2015.
 */
case class Variable(var name: String) extends Term with Comparable[Variable]{
  override def toString = name

  override def getFreeVariables(vars: Set[Variable]): Set[Variable] = vars + this

  override def compareTo(o: Variable): Int = this.name.compareTo(o.name)

  override def renameVariable(v: Variable): Unit = {
    if (v.name.equals(name)) {
      name = name.concat("'")
    }
  }

  override def getChainedVariables(vars: Set[Variable]): Set[Variable] = vars

  override def getAllVariables(vars: Set[Variable]): Set[Variable] = vars + this
}

