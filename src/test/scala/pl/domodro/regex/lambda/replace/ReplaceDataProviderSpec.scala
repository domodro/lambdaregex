package pl.domodro.regex.lambda.replace

import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class ReplaceDataProviderSpec extends FlatSpec with MockFactory {

  private trait TestFunctionParser extends FunctionParser {
    override def parse(function: String): Function[String, String] = mockedFunction.apply(function)
  }

  private case class Target(name: String, shouldContainFunction: Boolean = true)

  private val returnedFunction: Function[String, String] = value => value

  private val mockedFunction = mockFunction[String, Function[String, String]]

  private val replaceDataProvider = new ReplaceDataProvider with TestFunctionParser

  "$1" should "return no function" in
    test("$1",
      List(),
      List(Target("$1", shouldContainFunction = false)))

  "$1{_ => _.toUpperCase}" should "return function" in
    test("$1{_ => _.toUpperCase}",
      List("_ => _.toUpperCase"),
      List(Target("$1")))

  "$1{_ => _.toUpperCase} $2{_ => _.toLowerCase}" should "return functions" in
    test("$1{_ => _.toUpperCase} $2{_ => _.toLowerCase}",
      List("_ => _.toUpperCase", "_ => _.toLowerCase"),
      List(Target("$1"), Target("$2")))

  "$1{_ => _.toUpperCase.toLowerCase}" should "return function" in
    test("$1{_ => _.toUpperCase.toLowerCase}",
      List("_ => _.toUpperCase.toLowerCase"),
      List(Target("$1")))

  "$1{_ => _.toUpperCase}$2{_ => _.toLowerCase}" should "return functions" in
    test("$1{_ => _.toUpperCase}$2{_ => _.toLowerCase}",
      List("_ => _.toUpperCase", "_ => _.toLowerCase"),
      List(Target("$1"), Target("$2")))

  "$1{_ => _.toUpperCase}$2$3{_ => _.toLowerCase}" should "return functions" in
    test("$1{_ => _.toUpperCase}$2$3{_ => _.toLowerCase}",
      List("_ => _.toUpperCase", "_ => _.toLowerCase"),
      List(Target("$1"), Target("$2", shouldContainFunction = false), Target("$3")))

  private def test(replaceString: String, expectedFunctions: Seq[String], targets: Seq[Target]): Unit = {
    expectedFunctions.foreach(mockedFunction.expects(_).returns(returnedFunction))
    val returned = replaceDataProvider.findFunctions(replaceString)
    returned.size shouldBe targets.size
    targets.zip(returned).foreach(zip => {
      val target = zip._1
      val data = zip._2
      data.name shouldBe target.name
      if (target.shouldContainFunction) data.function shouldBe returnedFunction
    })
  }

}
