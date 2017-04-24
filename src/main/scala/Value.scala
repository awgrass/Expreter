import Interpreter.VariableName

sealed abstract class Value

//TODO Add case classes to represent the Values
case class intValue(i: Int) extends Value
case class refValue(i: Int) extends Value
case class listValue(l: List[Value]) extends Value
//case class letValue(m: Map[String, Value]) extends Value
//case class boolValue(b: Boolean) extends Value

object PrettyPrinter {
  //TODO This method should produce formated output for the output values

  //An Integer is printed as the Integer itself e.g. 42

  //A List is printed as the printed Elements separated by a colon and enclosed with square brackets e.g. [42,[],1]
  //  Scala's mkString function might be helpful to do this.

  //A Reference is printed as the address prefixed with $ e.g. $2
  def formatList(l: List[Value], s: String): String = {
    var newString:String = s
    for( i <- l) newString = newString + "," + print(i)
    return newString.substring(1)
  }

  def print(value: Value):String = value match {
    case intValue(value) => value.toString
    case refValue(value) => value.toString
    case listValue(value) => "[" + formatList(value, "") + "]"
    case null => "Returning..."



  }

}
