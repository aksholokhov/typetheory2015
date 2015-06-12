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
    val HOME_DIR = "/home/alexsholokhov/Документы/typetheory2015/homework/"
    val parser = new NonTypedLambdaParser
    Source.fromFile(HOME_DIR + "tests/task3/test1.in").getLines().map(_.toString.replace(" ", "_")) .map(parser.parseAll(parser.condition, _)
      .get).map({
        case Condition(where, v, what) =>
          subst(where, v, what, new TreeSet[Variable]()) match {
            case Some(a) => a.toString
            case None => "Нет свободы для подстановки для переменной " + v
          }
        case _ => "Invalid format"
    }).foreach(println(_))
  }
}
