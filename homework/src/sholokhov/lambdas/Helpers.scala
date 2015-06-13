package sholokhov.lambdas

import sholokhov.lambdas.parser.ArithmeticTerms._
import sholokhov.lambdas.parser._

import scala.collection.immutable.TreeSet
import scala.collection.mutable

object Helpers {
  def subst(where: Term, x: Variable, t: Term, boundedVars: Set[Variable]): Option[Term] = where match {
    case v: Variable => if (x.name.equals(v.name)) {
      if (boundedVars.intersect(t.getFreeVariables(new TreeSet[Variable]())).isEmpty && !boundedVars.contains(v)) Some(t) else None
    } else Some(x)
    case Application(a, b) => (subst(a, x, t, boundedVars), subst(b, x, t, boundedVars)) match {
      case (None, Some(_)) | (Some(_), None) | (None, None) => None
      case (Some(k), Some (p)) => Some(Application(k, p))
    }
    case Lambda(va, body) => subst(body, x, t, boundedVars + va) match {
      case None => None
      case Some(body2) => Some(Lambda(va, body2))
    }
  }
  
  def force_subst(where: Term, x: Variable, t: Term, boundedVars: Set[Variable]): Term = where match {
    case v: Variable => if (x == v && !boundedVars.contains(x)) t else v
    case Lambda(v, body) =>
      if (t.getAllVariables(new TreeSet[Variable]()).contains(v)) t.renameVariable(v)
      Lambda(v, force_subst(body, x, t, boundedVars + v))
    case Application(a, b) => Application(force_subst(a, x, t, boundedVars), force_subst(b, x, t, boundedVars))
  }
  
  def normalizeTerm(what: Term): Term = what match {
    case Application(a, b) => normalizeTerm(a) match {
      case Lambda(v, body) =>
        normalizeTerm(force_subst(body, v, b, new TreeSet[Variable]()))
      case n => Application(a, normalizeTerm(b))
    }
    case Lambda(v, body) => Lambda(v, normalizeTerm(body))
    case _ => what
  }

  def normalizeTermAtOnce(what: Term): Term = {
    what match {
      case Application(a, b) => a match {
        case Lambda(v, body) => force_subst(body, v, b, new TreeSet[Variable]())
        case app: Application => Application(normalizeTermAtOnce(a), b)
      }
      case _ => what
    }
  }

  def termToList(term: Term): List[Term] = term match {
    case v: Variable => List[Term](v)
    case t@Lambda(v, body) => List[Term](t) ::: termToList(body)
    case t@Application(t1, t2) => List[Term](t) ::: termToList(t1) ::: termToList(t2)
  }

  def constructRestrictions(list: List[Term]): (Option[List[Equation]], mutable.HashMap[Term, ArithmeticVariable]) = {
    val vargen = new VarGen
    val map = new mutable.HashMap[Term, ArithmeticVariable]()

    def assign(t: Term): ArithmeticVariable =
      if (map.get(t).isDefined) map.get(t).get else {
        val a = vargen.getNew
        map.put(t, a)
        a
      }

    (unify(list.map({
      case v: Variable => Equation(assign(v), assign(v))
      case l@Lambda(v, body) => Equation(assign(l), Function("f", List[ArithmeticTerm](assign(v), assign(body))))
      case t@Application(t1, t2) => Equation(assign(t1), Function("f", List[ArithmeticTerm](assign(t2), assign(t))))
    })), map)
  }

  def reconstructType(term: Term, eqs: List[Equation], map: mutable.HashMap[Term, ArithmeticVariable]): ArithmeticTerm = {
    var t0 = eqs.head.b
    eqs.foreach(eq => t0 = arithm_subst(t0, eq.a.asInstanceOf[ArithmeticVariable], eq.b))
    t0
  }

  def printAsType(expr: ArithmeticTerm): String = expr match {
    case ArithmeticVariable(name) => name
    case Function("f", a :: b :: tail) => "(" + printAsType(a) + "->" + printAsType(b) + ")"
  }
}
