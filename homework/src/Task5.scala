import java.io.PrintWriter
import ArithmeticTerms._

import scala.io.Source

/**
 * Created by alexsholokhov on 12.06.15.
 */
object Task5 {
  def main(args: Array[String]) {
    val parser = new ArithmeticParser
    val out = new PrintWriter("task5.out")
    val ans = unify(Source.fromFile("task5.in").getLines() .map(parser.parseAll(parser.equation, _)
      .get).toList, 0)
    if (ans.isDefined) ans.get.foreach(out.println)
    else out.println("no solution")
    out.close()
  }
}
