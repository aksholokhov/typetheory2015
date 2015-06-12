package sholokhov.lambdas

import sholokhov.lambdas.Helpers._
import sholokhov.lambdas.parser.ArithmeticParser
import sholokhov.lambdas.parser.ArithmeticTerms._

import scala.collection.immutable.TreeSet
import scala.io.Source

/**
 * Created by alexsholokhov on 12.06.15.
 */
object TestTask {
  def main(args: Array[String]) {
    val HOME_DIR = "/home/alexsholokhov/Документы/typetheory2015/homework/"
    val parser = new ArithmeticParser
    Source.fromFile(HOME_DIR + "tests/testtask/test1.in").getLines().map(_.toString.replace(" ", "_")) .map(parser.parseAll(parser.substitution, _)
      .get).map(x => arithm_subst(x.where, x.x, x.what)).foreach(println(_))
  }
}
