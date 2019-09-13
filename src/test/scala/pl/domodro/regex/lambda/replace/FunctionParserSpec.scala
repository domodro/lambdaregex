package pl.domodro.regex.lambda.replace

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class FunctionParserSpec extends FlatSpec {

  class TestFunctionParser extends FunctionParser {}

  private val functionParser = new TestFunctionParser()

  "String without function" should "return no function" in checkFunction("test")

  "x => x" should "return x" in checkFunction("x => x", "test", "test")

  "x =>x" should "return x" in checkFunction("x =>x", "test", "test")

  "x=>x" should "return x" in checkFunction("x=>x", "test", "test")

  "x=> x" should "return x" in checkFunction("x=> x", "test", "test")

  "x -> x" should "return x" in checkFunction("x -> x", "test", "test")

  "_ => _" should "return _" in checkFunction("_ => _", "test", "test")

  "x => x.toUpperCase" should "return x.toUpperCase" in checkFunction("x => x.toUpperCase", "test", "TEST")

  "x => x.toUpperCase()" should "return x.toUpperCase" in checkFunction("x => x.toUpperCase()", "test", "TEST")

  "_ => _.toUpperCase" should "return _.toUpperCase" in checkFunction("_ => _.toUpperCase", "test", "TEST")

  "x => x.toUpperCase.toLowerCase" should "return x" in checkFunction("x => x.toUpperCase.toLowerCase", "test", "test")

  "x => x.substring(0, 3)" should "return tes" in checkFunction("x => x.substring(0, 3)", "test", "tes")

  "x => x.substring(0, 3).concat(\"test\")" should "return testest" in checkFunction("x => x.substring(0, 3).concat(\"test\")", "test",
    "testest")

  private def checkFunction(testFunction: String, testValue: String, expected: String): Unit = {
    val function = functionParser.parse(testFunction)
    function.apply(testValue) shouldBe expected
  }

  private def checkFunction(testFunction: String): Unit = {
    val function = functionParser.parse(testFunction)
  }
}
