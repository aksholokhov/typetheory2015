package sholokhov.lambdas


import java.io.PrintWriter

import sholokhov.lambdas.parser.NonTypedLambdaParser
import sholokhov.lambdas.Helpers._
import sholokhov.lambdas.parser.ArithmeticTerms._

import scala.io.Source

/**
 * Created by alexsholokhov on 12.06.15.
 */
object Task6 {
  def main(args: Array[String]) {
    val parser = new NonTypedLambdaParser()
    val out = new PrintWriter("task6.out")
    Source.fromFile("task6.in").getLines().map(_.toString.replace(" ", "_"))
      .map( parser.parseAll(parser.expression, _).get).map(t => constructRestrictions(termToList(t)) match {
        case (None, _) => "Not typeable"
        case (Some(a), map) => printAsType(reconstructType(t, a, map))
    }).foreach(out.println)
    out.close()
  }
}
