package pl.domodro.regex.lambda.replace

trait FunctionParser {

  private val string = new String

  private val lambdaPattern = """([\w\d_]+)\s*[-=]>\s*(.*?)""".r

  private val lambdaBodyPattern = """(([\w\d_]+\(?['"\w\d_,\s]*\)?\.)+)?([\w\d_]+\(?['"\w\d_,\s]*\)?)\.?""".r

  private val functionCallPattern = """([\w\d_]+)\((['"\w\d_,\s]*)\)""".r

  private val argumentPattern = """(['"\w\d_\s]+),?(.*)""".r

  private val stringPattern = """"(.*)"""".r

  private val intPattern = """(-?[0-9.]+)""".r

  def parse(function: String): Function[String, String] = {
    function match {
      case lambdaPattern(key, value) => parse(key, value)
      case _ => value => value
    }
  }

  private def parse(varName: String, function: String): Function[String, String] = function match {
    case lambdaBodyPattern(_, _, lastFunction) if lastFunction.equals(varName) => value => value
    case lambdaBodyPattern(prevFunctions, _, lastFunction) => value => toFunction(lastFunction).apply(parse(varName, prevFunctions).apply(value))
  }

  private def toFunction(lastFunction: String): Function[String, String] = {
    val functionName = lastFunction match {
      case functionCallPattern(value, _) => value
      case value => value
    }
    val arguments: List[Any] = lastFunction match {
      case functionCallPattern(_, args) => getArguments(args)
      case _ => List[Any]()
    }
    string.getClass.getMethods
      .filter(method => method.getParameterCount.equals(arguments.size))
      .find(method => method.getName.equals(functionName)) match {
      case Some(method) => value: String => method.invoke(value, arguments:_*).asInstanceOf[String]
      case None => value: String => value
    }
  }

  private def getArguments(arguments: String): List[Any] = {
    arguments match {
      case argumentPattern(first, other) if other.isEmpty => argumentType(first) :: Nil
      case argumentPattern(first, other) => argumentType(first) :: getArguments(other)
      case _ => List()
    }
  }

  private def argumentType(argument: String): Any = argument.trim match {
    case stringPattern(value) => value
    case intPattern(value) => Integer.parseInt(value)
    case value => value
  }
}
