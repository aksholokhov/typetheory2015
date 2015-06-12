package sholokhov.lambdas

import sholokhov.lambdas.parser.ArithmeticParser
import sholokhov.lambdas.parser.ArithmeticTerms._

import scala.io.Source

/**
 * Created by alexsholokhov on 12.06.15.
 */
object Task5 {
  def main(args: Array[String]) {
    val HOME_DIR = "/home/alexsholokhov/Документы/typetheory2015/homework/"
    val parser = new ArithmeticParser
    val ans = unify(Source.fromFile(HOME_DIR + "tests/task5/test1.in").getLines() .map(parser.parseAll(parser.equation, _)
      .get).toList)
    if (ans.isDefined) ans.get.foreach(println(_))
    else println("no solution")
  }
}
