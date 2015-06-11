package sholokhov.lambdas.parser

import scala.collection.immutable.HashSet

/**
 * Created by Шолохов on 27.05.2015.
 */
case class Variable(name: String) extends Term with Comparable[Variable]{
  override def toString = name

  override def getFreeVariables(vars: Set[Variable]): Set[Variable] = vars + this

  override def compareTo(o: Variable): Int = this.name.compareTo(o.name)

  override def renameVariable(v: Variable): Unit = {
    if (v.name.equals(name)) {
      this.name += "'"
    }
  }

  override def getChainedVariables(vars: Set[Variable]): Set[Variable] = vars
}
