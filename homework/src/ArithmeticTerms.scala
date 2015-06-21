import scala.collection.mutable

/**
 * Created by alexsholokhov on 12.06.15.
 */
object ArithmeticTerms {
  trait ArithmeticTerm extends Comparable[ArithmeticTerm]

  case class Equation (a: ArithmeticTerm, b: ArithmeticTerm) extends ArithmeticTerm {
    override def compareTo(o: ArithmeticTerm): Int = o match {
      case Equation(a1, b1) if a1.compareTo(a) == 0 && b1.compareTo(b) == 0 => 0
      case _ => -1
    }

    override def toString: String = a.toString + "=" + b.toString
  }

  case class Function (name: String, args: List[ArithmeticTerm]) extends ArithmeticTerm {
    override def compareTo(o: ArithmeticTerm): Int = o match {
      case Function(n, a) if a.size == args.size && n.equals(name) => args.zip(a).foldLeft(0)((ans, x) => if (ans != 0) ans else x._1.compareTo(x._2))
      case _ => -1
    }

    override def toString: String = name + "(" + args.head.toString + args.tail.foldLeft("")((str, e) => str + "," + e) + ")"
  }
  case class ArithmeticVariable (name: String) extends ArithmeticTerm {
    override def compareTo(o: ArithmeticTerm): Int = o match {
      case ArithmeticVariable(n) => n.compareTo(name)
      case _ => -1
    }

    override def toString: String = name
  }

  case class ArithmeticSubstitution(where: ArithmeticTerm, x: ArithmeticVariable, what: ArithmeticTerm) extends ArithmeticTerm {
    override def compareTo(o: ArithmeticTerm): Int = ???
  }

  case class ArithmeticConstant(name: String) extends ArithmeticTerm {
    override def compareTo(o: ArithmeticTerm): Int = o match {
      case ArithmeticConstant(name2) => name.compareTo(name2)
      case _ => -1
    }
  }

  class ConGen {
    var counter = 0
    def getNew = new ArithmeticConstant("t" + (counter+=1))
  }

  class VarGen {
    var counter = 0
    def getNew =  {
      counter += 1
      new ArithmeticVariable("x" + counter)
    }
  }
/*
  def unify(c: List[Equation], n: Int): Option[List[Equation]] = c match {
    case Equation(s, t) :: tail => {
      if (c.size == n) Some(c) else
      if (s == t) unify(tail, 0)
      else {
        (s, t) match {
          case (x: ArithmeticVariable, t: ArithmeticTerm) => if (!getVars(t).contains(x) & tail.foldLeft(false)((ans, eq) => getVars(eq).contains(x) | ans)) {
            unify(arithm_subst(tail, x, t) ::: List[Equation](Equation(x, t)), 0)
          } else None
          case (t: ArithmeticTerm, x : ArithmeticVariable) => unify(Equation(x, t) :: tail, 0)
          case (Function(s_name, s_args), Function(t_name, t_args)) => if (s_args.size != t_args.size | !s_name.equals(t_name) ) None else {
            unify(s_args.zip(t_args).map(x => Equation(x._1, x._2)) ::: tail, 0)
          }
        }
      }
    }
    case a :: b :: tail => unify((b :: tail) ::: List[Equation](a), n + 1)
    case Nil => Some(Nil)
  }
  */

  def unify(c: List[Equation], n: Int): Option[List[Equation]] =
    if (c.size == n) Some(c) else c match {
    case Equation(s, t) :: tail =>
        (s, t) match {
          case _ if s == t => unify(tail, 0)
          case (x: ArithmeticVariable, t: ArithmeticTerm) if !getVars(t).contains(x) & (tail == Nil | tail.foldLeft(false)((ans, eq) => getVars(eq).contains(x))) =>
            val pp = arithm_subst(tail, x, t)
            unify(pp, 0) match {
              case Some(ans) => Some(Equation(x, t) :: ans)
              case None => None
            }
          case (t: ArithmeticTerm, x : ArithmeticVariable) => unify(Equation(x, t) :: tail, 0)
          case (Function(s_name, s_args), Function(t_name, t_args)) => if (s_args.size != t_args.size | !s_name.equals(t_name) ) None else {
            unify(s_args.zip(t_args).map(x => Equation(x._1, x._2)) ::: tail, 0)
          }
          case _ => c match {
            case a :: b :: last => unify((b :: last) ::: List[Equation](a), n + 1)
            case Nil => Some(Nil)
          }
        }
  }

/*
  def robinsonUnify(c: List[Equation]): Option[List[Equation]] = c match {
    case Equation(s, t) :: tail => {
      if (s == t) unify(tail)
      else {
        (s, t) match {
          case (s1: ArithmeticConstant, f1: Function) | (f: Function, s: ArithmeticConstant) => None
          case (ArithmeticConstant(t1), ArithmeticConstant(t2)) if !t1.equals(t2) => None
          case (s: ArithmeticTerm, t: ArithmeticVariable) => robinsonUnify(Equation(t, s) :: tail)
          case (x: ArithmeticVariable, t: ArithmeticTerm) => if (getVars(t).contains(x)) None else robinsonUnify(arithm_subst(tail, x, t)) match {
            case None => None
            case Some(ans) => Some(Equation(x, t) :: ans)
          }
          case (Function(s_name, s_args), Function(t_name, t_args)) => if (!s_name.equals(t_name)) None else {
            robinsonUnify(s_args.zip(t_args).map(x => Equation(x._1, x._2)) ::: tail)
          }
        }
      }
    }
    case Nil => Some(Nil)
  }
*/
  def arithm_subst(eqs: List[Equation], v: ArithmeticVariable, t: ArithmeticTerm): List[Equation] = eqs.map(
    x => Equation(arithm_subst(x.a , v, t), arithm_subst(x.b, v, t))
  )

  def arithm_subst(where: ArithmeticTerm, v: ArithmeticVariable, t: ArithmeticTerm): ArithmeticTerm = where match {
    case Function(name, args) => Function(name, args.map(arithm_subst(_, v, t)))
    case x: ArithmeticVariable if x.name.equals(v.name) => t
    case x => x
  }

  def getVars(where: ArithmeticTerm): mutable.HashSet[ArithmeticVariable] = where match {
    case e: Equation => getVars(e.a) ++ getVars(e.b)
    case v: ArithmeticVariable => new mutable.HashSet[ArithmeticVariable]() + v
    case t: ArithmeticConstant => new mutable.HashSet[ArithmeticVariable]()
    case Function(name, args) => args.foldLeft(new mutable.HashSet[ArithmeticVariable]())((set, e) => set ++ getVars(e))
  }
}
