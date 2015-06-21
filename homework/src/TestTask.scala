import java.util

import Helpers._

import scala.collection.immutable.TreeSet
import scala.io.Source

/**
 * Created by alexsholokhov on 12.06.15.
 */
object TestTask {
  def main(args: Array[String]) {
    val parser = new NonTypedLambdaParser
    Source.fromFile("test1.in").getLines().map(_.toString.replace(" ", "_")) .map(parser.parseAll(parser.expression, _)
      .get).map(x => uniquefyVars(x, new TreeSet[Variable]())).foreach(x => println(x._1))
  }
}
