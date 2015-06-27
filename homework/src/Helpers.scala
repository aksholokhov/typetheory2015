import ArithmeticTerms._

import scala.collection.immutable.TreeSet
import scala.collection.mutable

object Helpers {
  def subst(where: Term, x: Variable, t: Term, boundedVars: Set[Variable]): Option[Term] = where match {
    case v: Variable => if (x.name.equals(v.name)) {
      if (boundedVars.intersect(t.getFreeVariables(new TreeSet[Variable]())).isEmpty && !boundedVars.contains(v)) Some(t) else None
    } else Some(x)
    case Application(a, b) => (subst(a, x, t, boundedVars), subst(b, x, t, boundedVars)) match {
      case (None, Some(_)) | (Some(_), None) | (None, None) => None
      case (Some(k), Some(p)) => Some(Application(k, p))
    }
    case Lambda(va, body) => subst(body, x, t, boundedVars + va) match {
      case None => None
      case Some(body2) => Some(Lambda(va, body2))
    }
  }

  def simpleSubst(where: Term, x: Variable, t: Term): Term = where match {
    case v: Variable => if (x.name.equals(v.name)) t else x
    case Application(a, b) => Application(simpleSubst(a, x, t), simpleSubst(b, x, t))
    case Lambda(va, body) => simpleSubst(body, x, t)
  }


  case class Substitution(a: Term, v: Variable, b: Term) extends Term {
    override def getFreeVariables(vars: Set[Variable]): Set[Variable] = ???

    override def getChainedVariables(vars: Set[Variable]): Set[Variable] = ???

    override def renameVariable(v: Variable): Unit = ???

    override def getAllVariables(vars: Set[Variable]): Set[Variable] = ???
  }

  //TODO: rename fix
  def force_subst(where: Term, x: Variable, t: Term, boundedVars: Set[Variable]): Term = where match {
    case v: Variable => if (x == v && !boundedVars.contains(x)) t else v
    case Lambda(v, body) =>
      if (t.getAllVariables(new TreeSet[Variable]()).contains(v)) t.renameVariable(v)
      Lambda(v, force_subst(body, x, t, boundedVars + v))
    case Application(a, b) => Application(force_subst(a, x, t, boundedVars), force_subst(b, x, t, boundedVars))
  }

  def parallelReduce(what: Term, map: mutable.HashMap[Term, Term]): Term = what match {
    case Application(Lambda(v, body), t2) =>
      val s = Substitution(body, v, t2)
      if (map.contains(s))
        map.get(s).get
      else {
        val res = simpleSubst(body, v, t2)
        map.put(s, res)
        res
      }
    case Application(t1, t2) => Application(parallelReduce(t1, map), parallelReduce(t2, map))
    case Lambda(v, body) => Lambda(v, parallelReduce(body, map))
    case v: Variable => v
  }

  def normalizeTerm(what: Term, map: mutable.HashMap[Term, Term]): Term = {
    val tt = parallelReduce(what, map)
    //  print(tt)
    if (tt == what) what else normalizeTerm(tt, map)
    /*    case Application(a, b) => normalizeTerm(a) match {
      case Lambda(v, body) =>
        normalizeTerm(force_subst(body, v, b, new TreeSet[Variable]()))
      case n => Application(a, normalizeTerm(b))
    }
    case Lambda(v, body) => Lambda(v, normalizeTerm(body))
    case _ => what */
  }

  def termToList(term: Term): List[Term] = term match {
    case v: Variable => List[Term](v)
    case t@Lambda(v, body) => List[Term](t) ::: termToList(body)
    case t@Application(t1, t2) => List[Term](t) ::: termToList(t1) ::: termToList(t2)
  }

  def uniquefyVars(term: Term, used: Set[Variable]): (Term, Set[Variable]) = term match {
    case t@Application(t1, t2) =>
      val nt1 = uniquefyVars(t1, used)
      val nt2 = uniquefyVars(t2, nt1._2)
      (Application(nt1._1, nt2._1), nt2._2)

    case l@Lambda(v, body) => if (used.contains(v)) {
      val n = genNewName(v, used)
      renameVariable(body, v, n)
      renameVariable(v, v, n)
    }
      val nt = uniquefyVars(body, used + v)
      (l, nt._2)

    case v: Variable => (v, used)
  }

  def genNewName(v: Variable, used: Set[Variable]): String = {
    var name = v.name
    while (used.contains(Variable(name))) name += "'"
    name
  }

  def renameVariable(term: Term, v: Variable, newName: String): Unit = term match {
    case Application(t1, t2) => renameVariable(t1, v, newName); renameVariable(t2, v, newName)
    case Lambda(lv, body) => renameVariable(lv, v, newName); renameVariable(body, v, newName)
    case t: Variable => if (t == v) t.name = newName
  }

  def constructRestrictions(list: List[Term]): (Option[List[Equation]], mutable.HashMap[Term, ArithmeticVariable]) = {

    val vargen = new VarGen
    val map = new mutable.HashMap[Term, ArithmeticVariable]()

    def assign(t: Term): ArithmeticVariable =
      if (map.get(t).isDefined) map.get(t).get
      else {
        val a = vargen.getNew
        map.put(t, a)
        a
      }

    val constr = list.map({
      case v: Variable => Equation(assign(v), assign(v))
      case l@Lambda(v, body) => {
        val lname = assign(l)
        val vname = assign(v)
        val bodyname = assign(body)
        Equation(lname, Function("f", List[ArithmeticTerm](vname, bodyname)))
      }
      case t@Application(t1, t2) => {
        val appname = assign(t)
        val t1name = assign(t1)
        val t2name = assign(t2)
        Equation(t1name, Function("f", List[ArithmeticTerm](t2name, appname)))
      }
    })
   // constr.foreach(println(_))
    (unify(constr, 0), map)
  }

  def reconstructType(term: Term, eqs: List[Equation], map: mutable.HashMap[Term, ArithmeticVariable]): ArithmeticTerm = {
    val t0 = map.get(term).get
    eqs.foreach(eq => if (eq.a == t0) return eq.b)
    t0
  }

  def printAsType(expr: ArithmeticTerm): String = expr match {
    case ArithmeticVariable(name) => name
    case Function("f", a :: b :: tail) => "(" + printAsType(a) + "->" + printAsType(b) + ")"
  }
}
