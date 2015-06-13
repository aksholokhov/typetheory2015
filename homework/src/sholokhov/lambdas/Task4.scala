package sholokhov.lambdas

import java.io.PrintWriter

import sholokhov.lambdas.Helpers._
import sholokhov.lambdas.parser.{Variable, Condition, NonTypedLambdaParser}

import scala.collection.immutable.TreeSet
import scala.io.Source

/**
 * Created by alexsholokhov on 12.06.15.
 */
object Task4 {
  def main (args: Array[String]) {
    val HOME_DIR = "/home/alexsholokhov/Документы/typetheory2015/homework/"
    val parser = new NonTypedLambdaParser
    val out = new PrintWriter(HOME_DIR + "tests/task4/task4.out")
    Source.fromFile(HOME_DIR + "tests/task4/task4.in").getLines().map(_.toString.replace(" ", "_")) .map(parser.parseAll(parser.expression, _)
      .get).map(normalizeTerm).foreach(out.println)
    out.close()
  }
}
