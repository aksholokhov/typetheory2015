package sholokhov.lambdas

import sholokhov.lambdas.parser.{Variable, Term, Condition, NonTypedLambdaParser}

import scala.collection.immutable.TreeSet
import scala.io.Source

/**
 * Created by Шолохов on 30.05.2015.
 */
object Task3 {
  def main(args: Array[String]) {
    val parser = new NonTypedLambdaParser
    Source.fromFile("tests/task3").getLines().map(parser.parseAll(parser.condition, _)
      .get)map(x => {
      x match {
        case Condition(where, v, what) => {
          val freeVars = what.getFreeVariables(new TreeSet[Variable])
          
        }
        case _ => "Invalid format"
      }
    })
  }
}
