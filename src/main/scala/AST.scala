case class Program(functionEnvironment: List[FunctionDeclaration], main:Expression)

case class FunctionDeclaration(name:String, params: List[String], body: Expression)

case class Predicate(name: String, params: List[Expression])

sealed abstract class Expression

//TODO Add case classes to represent the Expression AST

case class condExp(p: Predicate, e1: Expression, e2: Expression) extends Expression
case class functionExp(id: String, params: List[Expression]) extends Expression
case class intExp(i: Int) extends Expression
case class listExp(l: List[Expression]) extends Expression
case class varExp(id: String ) extends Expression

case class seqExp(e: List[Expression]) extends Expression

//let bindings
case class letExp(let: List[Expression], e: Expression) extends Expression
case class bindExp(id: String, v: Expression) extends Expression
case class refbindExp(id: String, v: Expression) extends Expression

//refs
case class derefExp(id: String) extends Expression
case class assExp(id: String, e: Expression) extends Expression

//built in functions
case class add(a: Expression, b: Expression ) extends Expression
case class negate(a: Expression) extends Expression
case class first(xs: Expression) extends Expression
case class rest(xs: Expression) extends Expression
case class build(x: Expression, xs: Expression) extends Expression

//conditionals
case class eqCond(params: List[Expression], e1:Expression, e2: Expression) extends Expression
case class ltCond(params: List[Expression], e1:Expression, e2: Expression) extends Expression

//predicates
case class eq(a: Expression, b: Expression) extends Expression
case class lt(a: Expression, b: Expression) extends Expression
