package sholokhov.lambdas

import java.io.PrintWriter

import sholokhov.lambdas.parser.{NonTypedLambdaParser}

import scala.io.Source

object Task1 {
  def main(args: Array[String]) {
    val parser = new NonTypedLambdaParser()
    val HOME_DIR = "/home/alexsholokhov/Документы/typetheory2015/homework/"
    val out = new PrintWriter(HOME_DIR + "tests/task1/task1.out")
    Source.fromFile(HOME_DIR + "tests/task1/task1.in").getLines().map(_.toString.replace(" ", "_"))
      .map( parser.parseAll(parser.expression, _).get).foreach(out.println)
    out.close()
  }
}
