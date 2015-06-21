import java.util

import Helpers._

import scala.collection.immutable.TreeSet
import scala.io.Source
import ArithmeticTerms._

/**
 * Created by alexsholokhov on 12.06.15.
 */
object TestTask {
  def main(args: Array[String]) {
    val parser = new ArithmeticParser
    Source.fromFile("test1.in").getLines().map(_.toString.replace(" ", "_")) .map(parser.parseAll(parser.substitution, _)
      .get).map(x => arithm_subst(x.where, x.x, x.what)).foreach(println(_))
  }
}
