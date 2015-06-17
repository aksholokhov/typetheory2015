package sholokhov.lambdas

import java.io.{File, PrintWriter}
import java.nio.file.Path

import sholokhov.lambdas.parser.{Variable, Term, Condition, NonTypedLambdaParser}
import sholokhov.lambdas.Helpers._
import scala.collection.immutable.TreeSet
import scala.io.Source

/**
 * Created by Шолохов on 30.05.2015.
 */
object Task3 {
  def main(args: Array[String]) {
    val parser = new NonTypedLambdaParser
    val out = new PrintWriter("task3.out")
    Source.fromFile("task3.in").getLines().map(_.toString.replace(" ", "_")) .map(parser.parseAll(parser.condition, _)
      .get).map({
        case Condition(where, v, what) =>
          subst(where, v, what, new TreeSet[Variable]()) match {
            case Some(a) => a.toString
            case None => "Нет свободы для подстановки для переменной " + v
          }
        case _ => "Invalid format"
    }).foreach(out.println)
    out.close()
  }
}
