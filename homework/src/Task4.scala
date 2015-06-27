import java.io.PrintWriter

import Helpers._

import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.io.Source

/**
 * Created by alexsholokhov on 12.06.15.
 */
object Task4 {
  def main (args: Array[String]) {
    val parser = new NonTypedLambdaParser
    val out = new PrintWriter("task4.out")
    Source.fromFile("task4.in").getLines().map(_.toString.replace(" ", "_")) .map(parser.parseAll(parser.expression, _)
      .get)
      //.map(uniquefyVars(_, new TreeSet[Variable]())._1)
      .map(x => normalizeTerm(x, new mutable.HashMap[Term, Term]()))
      .foreach(out.println)
    out.close()
  }
}
