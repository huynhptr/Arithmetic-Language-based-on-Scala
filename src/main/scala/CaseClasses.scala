/*
Each case class is a token for this language. They each will be executable and produce
a Value for an Expression, and a string that describes what is done for a Statement.
The 'tokenType' field is for identifying if a Token is an Expression or a Statement so downcastings would
work correctly.
 */
class Token(val tokenType: String)

class Expression(t: String = "Expression") extends Token(t)

case class Type(typeName: String) extends Expression
case class Value(v: Any, t: Type) extends Expression {
  /*
  Define + for Values. Perform integer addition if both operands are int. Otherwise, perform strings concatenation
   */
  def +(other: Value): Value = (this.t.typeName, other.t.typeName) match {
    case ("int", "int") => Value(v.asInstanceOf[Int] + other.v.asInstanceOf[Int], Type("int"))
    case ("int", "string") | ("string", "int") | ("string", "string") =>
      Value(v.toString.concat(other.v.toString), Type("string"))
    case _ =>
      throw new Exception(s"No operator+ defined: +(${this.t.typeName} => ${other.t.typeName})!")
  }
  /*
  Define * for Values. Perform integer multiplication if both operands are int.
  If the first operand 's' is string and the second 'n' operand is int, perform n times concatenation of string s.
  Otherwise, an exception is thrown
   */
  def *(other: Value): Value = (this.t.typeName, other.t.typeName) match {
    case ("int", "int") => Value(v.asInstanceOf[Int] * other.v.asInstanceOf[Int], Type("int"))
    case ("string", "int") => Value(v.asInstanceOf[String] * other.v.asInstanceOf[Int], Type("string"))
    case _ =>
      throw new Exception(s"No operator* defined: *(${this.t.typeName} => ${other.t.typeName})!")
  }
}
case class Add(op1: Expression, op2: Expression) extends Expression
case class Multiply(op1: Expression, op2: Expression) extends Expression
case class Variable(name: String) extends Expression
case class Macro(name: String) extends Expression
case class Let(statement: Statement, expression: Expression) extends Expression


class Statement(t: String = "Statement") extends Token(t)

case class Assign(varName: String, e: Expression) extends Statement
case class DefineMacro(macroName: String, e: Expression) extends Statement
case class CreateScope(scopeName: String, s: Statement) extends Statement
case class CreateClass(className: String,
                       superClass: Option[Class] = None,
                       toBeNestedClasses: List[CreateClass] = List.empty) extends Statement
case class DeclareClassVars(className: String, vars: List[(String, Type)]) extends Statement
case class CreateClassMethod(ofClass: Class, methodName: String,
                             params: List[(String, Type)] = List.empty, codes: List[Token] = List.empty) extends Statement
case class AddNestedClass(existingClassName: String, toBeCreated: CreateClass) extends Statement
