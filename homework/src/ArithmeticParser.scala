import ArithmeticTerms._

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
/**
 * Created by alexsholokhov on 12.06.15.
 */
class ArithmeticParser extends RegexParsers with PackratParsers{
  lazy val number = "[0-9]+".r
  lazy val funname = "[a-h]".r
  lazy val varname = "[i-z]".r
  lazy val chars = "[a-z]".r

  lazy val substitution: PackratParser[ArithmeticSubstitution] = (term ~ ("[" ~> term <~ ":=" ) ~ (term <~ "]")) ^^ {
    case where ~ v ~ what => ArithmeticSubstitution(where, v.asInstanceOf[ArithmeticVariable], what)
  }

  lazy val equation: PackratParser[Equation] = (term ~ ("=" ~> term)) ^^ {
    case a ~ b => Equation(a, b)
  }

  lazy val term: PackratParser[ArithmeticTerm] = (funname ~ opt(rep1(chars | number | "'")) ~ ("(" ~> rep1sep(term, ",") <~ ")")) ^^ {
    case f ~ Some(ch) ~ args  => Function(f + ch.foldLeft("")((name, c) => name + c), args)
    case f ~ None ~ args => Function(f, args)
  } | varname ~ opt(rep1(chars | number | "'")) ^^ {
    case v ~ Some(ch) => ArithmeticVariable(v + ch.foldLeft("")((name, c) => name + c))
    case v ~ None => ArithmeticVariable(v)
  }
}
