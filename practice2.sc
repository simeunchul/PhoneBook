

def optionalLength1(parameter: Option[String]): Option[Int] =
  parameter.map(theString => theString.length)

optionalLength1(Option("hi"))

