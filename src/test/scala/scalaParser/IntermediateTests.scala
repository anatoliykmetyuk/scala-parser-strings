package scalaParser
package syntax

import scala.language.implicitConversions
import scala.util.{Failure, Success}

import org.scalatest._

import org.parboiled2._


class IntermediateTests extends FlatSpec with Matchers with IntermediateTestsHelper {

  behavior of "Strings"

  it should "simple" in checkStr(
    "\"Simple string\"",
    "\"Simple string\""
  )

  it should "interpolations" in checkStr(
    "s\"Hello $world\"",
    "s\"Hello $world\""
  )

}

trait IntermediateTestsHelper {this: IntermediateTests =>

  def check[P <: Parser](in: String, out: String, what: P => Rule1[String], parser: String => P) {
    val p = parser(in)
    p.__run(what(p)) match {
      case Failure(f: ParseError) => throw f
      case Success(parsed) => parsed shouldBe out
    }
  }

  def checkStr(in: String, out: String) = check(
    in     = in,
    out    = out,
    what   = {x: Literals => x.Literals.String},
    parser = {x => new Parser with Literals with Basic with Identifiers with RulesOps {def Block = ???; def WL = ???; val input: ParserInput = in}}// val input = x}}
  )

}