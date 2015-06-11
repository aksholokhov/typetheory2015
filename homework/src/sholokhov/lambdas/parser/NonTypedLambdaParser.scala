package sholokhov.lambdas.parser

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharArrayReader

class NonTypedLambdaParser extends RegexParsers with PackratParsers{
  lazy val number = "[0-9]+".r
  lazy val char = "[a-z]".r

  lazy val condition: PackratParser[Term] = (expression ~ ("[" ~> variable <~ ":=") ~ expression <~ "]") ^^ {
    case where ~ v ~ what => new Condition(where, v, what)
  }

  lazy val expression: PackratParser[Term] = (opt(substitution  <~ "_") ~ ("\\" ~>  variable <~ ".") ~ expression) ^^ {
    case None ~ v ~ body => Lambda(v, body)
    case Some(subst) ~ v ~ body => Substitution(subst, Lambda(v, body))
  } | substitution

  lazy val substitution: PackratParser[Term] = substitution ~ "_".r ~ atom  ^^ {
    case a ~ op ~ b => {
      Substitution(a, b)
    };
  } | atom

  lazy val atom: PackratParser[Term] = "(" ~> expression <~ ")" | variable

  lazy val variable: PackratParser[Variable] = (char ~ opt(number) ~ opt("'")) ^^ {
    case a~b~c => Variable(a + (if (b.isDefined) b.get else "") + (if (c.isDefined) b.get else ""))
  }

  def parseAll[T](p: Parser[T], input: String) = parse(p, new PackratReader(new CharArrayReader(input.toCharArray)))
}
