import java.io.PrintWriter

import scala.collection.immutable.TreeSet
import scala.io.Source

object Task2 {
  def main(args: Array[String]) {
    val parser = new NonTypedLambdaParser()
    val out = new PrintWriter("task2.out")
    Source.fromFile("task2.in").getLines().map(_.toString.replace(" ", "_"))
      .map( parser.parseAll(parser.expression, _).get).map(_.getFreeVariables(new TreeSet[Variable]()))
      .foldLeft(0)((num, set) => {out.print(num + ") "); set.foreach(v => out.print(v + ", ")); out.println(); num+1})
    out.close()
  }
}
