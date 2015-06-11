package sholokhov.lambdas

import java.io.PrintWriter

import sholokhov.lambdas.parser.{NonTypedLambdaParser}

import scala.io.Source

object Task1 {
  def main(args: Array[String]) {
    val parser = new NonTypedLambdaParser()
    val out = new PrintWriter("tests/task1/output")
    val str = Source.fromFile("tests/task1/test1.in").getLines().map(_.toString.replace(" ", "_"))
      .map( parser.parseAll(parser.expression, _).get).foreach(v => out.println("(" + v + ")"))
    out.close()
  }
}
