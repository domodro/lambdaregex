package pl.domodro.regex.lambda.replace

object ReplaceDataProvider {
  private val provider = new ReplaceDataProvider

  def apply(): ReplaceDataProvider = provider
}

class ReplaceDataProvider extends FunctionParser {

  private val replaceFinder = """(\$[0-9]+)""".r

  def findFunctions(replaceString: String): Seq[ReplaceData] = {
    replaceFinder
      .findAllMatchIn(replaceString)
      .flatMap(_.subgroups)
      .map(value => {
        val regex = s""".*\\$value\\{([a-zA-Z_]+\\s*[-=]>\\s*[\\w\\(\\)"'_\\.]*\\s*)\\}.*""".r
        replaceString match {
          case regex(function) => ReplaceData(value, parse(function))
          case _ => ReplaceData(value)
        }
      })
      .toSeq
  }
}
