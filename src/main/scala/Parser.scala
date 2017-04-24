import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  //Nonterminale
  val program: Parser[Program] = funcDeclarations ~ expression ^^ {case fs~e => Program(fs,e)}

  private def funcDeclarations : Parser[List[FunctionDeclaration]] = "{" ~> rep(funcDecl) <~ "}"

  private def funcDecl : Parser[FunctionDeclaration] =

    "fun" ~> identifier ~ "(" ~ repsep(identifier, ",") ~ ")" ~ "=" ~ expression <~ ";" ^^ {case n~_~xs~_~_~e => FunctionDeclaration(n,xs,e)}

  private def expression : Parser[Expression] =
  //TODO Implement the Expression Parser and add additional parsers for terminal and non terminal symbols, where necessary
    (let| sequence | deref | assign |cond | function | int | list | variable)

  private def let: Parser[Expression] =
    "let" ~> rep(refbinding | binding) ~ "in" ~ expression <~ "end" ^^ {case b~_~e => letExp(b, e) }

  private def deref: Parser[Expression] = "!" ~> identifier ^^{case v => derefExp(v)}

  private def assign: Parser[Expression] = identifier ~ ":="  ~ expression ^^{case id~_~e => assExp(id, e)}

  private def sequence: Parser[Expression] = "{" ~> repsep(expression, ";") <~ "}" ^^ {case e => seqExp(e)}

  private def binding: Parser[Expression] =
    "val" ~> identifier ~ "=" ~ expression <~ ";"  ^^ { case v~_~e => bindExp(v, e)}

  private def refbinding: Parser[Expression] =
    "val" ~> identifier ~ "=" ~ "ref(" ~ expression <~ ")" ~ ";"  ^^ { case v~_~_~e => refbindExp(v, e)}

  private def cond : Parser[Expression]  =
    "if" ~> predicate ~ "then" ~ expression ~ "else" ~ expression ^^ {case p~_~e1~_~e2 => condExp(p, e1, e2)}

  private def predicate: Parser[Predicate] =
    //TODO Implement a Parser for Predicate
    identifier ~ "?" ~ "(" ~ repsep(expression, ",") <~ ")" ^^ {case n~_~_~xs => Predicate(n, xs)}

  private def function: Parser[Expression] =
    identifier ~ "(" ~ repsep(expression, ",") <~ ")" ^^ {case n~_~xs  => functionExp(n, xs)}

  private def list: Parser[Expression] =
    "[" ~>  repsep(expression, ",") <~ "]" ^^ {case xs => listExp(xs)}

  private def variable: Parser[Expression] = identifier ^^ {case id => varExp(id)}

  //Terminale
  private val identifier : Parser[String] = "[a-z][A-Z|a-z|0-9]*".r

  private def int: Parser[Expression] = "0|[1-9][0-9]*|-[1-9][0-9]*".r ^^ {case x => intExp(x.toInt)}
}

object ParseProgram extends ExpParser {
  def parse(s: String): ParseResult[Program] = {
    parseAll(program, s)
  }
}
