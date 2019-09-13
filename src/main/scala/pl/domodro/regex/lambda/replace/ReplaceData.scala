package pl.domodro.regex.lambda.replace

case class ReplaceData(name: String, function: Function[String, String] = value => value)
