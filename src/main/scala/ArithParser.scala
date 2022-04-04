import scala.util.parsing.combinator.JavaTokenParsers
import Environment._

/*
A parser to process textual input and produce executable codes(Tokens).
Functions having name with prefix '_' are helpers, not corresponding to any case class.
 */
class ArithParser extends JavaTokenParsers {

  def tokenize(string: String): List[Token] = parseAll(_tokens, string) match {
    case Success(t, _) => t
    case Failure(msg, next) => throw new Exception(s"$msg: $next (CHECK input manually!!!)")
    case _ => throw new Exception("Error parsing!")
  }

  private def _tokens: Parser[List[Token]] = rep(token)
  private def token: Parser[Token] = expression | statement


  private def expression: Parser[Expression] = add | multiply | value | variable | mac | let | typ
  private def add: Parser[Add] = "Add("~>expression~","~expression<~")" ^^ {
    case op1~","~op2 => Add(op1,op2)
    case _ => throw new Exception("Error parsing!")
  }
  private def multiply: Parser[Multiply] = "Multiply("~>expression~","~expression<~")" ^^ {
    case op1~","~op2 => Multiply(op1,op2)
    case _ => throw new Exception("Error parsing!")
  }
  private def value: Parser[Value] = "Value("~>(floatingPointNumber | stringLiteral)~","~typ<~")" ^^ {
    case content~","~t =>
      t.typeName match {
        case "int" =>
          Value(content.toInt, t)
        case "string" => Value(content.stripPrefix("\"").stripSuffix("\""), t)
        case _ => throw new Exception(s"Error parsing, unknown type: ${t.typeName}!")
      }
    case _ => throw new Exception("Error parsing!")
  }
  private def typ: Parser[Type] = "Type("~>stringLiteral<~")" ^^ {
    case str @ ("\"int\"" | "\"string\"") => Type(_stripQuotes(str))
    case anythingElse => throw new Exception(s"Error parsing, unknown type: $anythingElse!")
  }
  private def variable: Parser[Variable] = "Variable("~>stringLiteral<~")" ^^
    (str => Variable(_stripQuotes(str)))
  private def mac: Parser[Macro] = "Macro("~>stringLiteral<~")" ^^ (str => Macro(str.stripPrefix("\"").stripSuffix("\"")))
  private def let: Parser[Let] = "Let("~>statement~")"~"In"~expression ^^ {
    case str~")"~"In"~exp => Let(str,exp)
    case _ => throw new Exception("Error parsing!")
  }


  private def statement: Parser[Statement] = assign | defineMacro | createScope | createClass | addNestedClass() |
    declareClassVars | createClassMethod
  private def assign: Parser[Assign] = "Assign("~>stringLiteral~","~expression<~")" ^^ {
    case str~","~exp => Assign(_obscuringCheck(_stripQuotes(str)),exp)
    case _ => throw new Exception("Error parsing!")
  }
  private def defineMacro: Parser[DefineMacro] = "DefineMacro("~>stringLiteral~","~expression<~")" ^^ {
    case str~","~exp => DefineMacro(_obscuringCheck(_stripQuotes(str)), exp)
    case _ => throw new Exception("Error parsing!")
  }
  private def createScope: Parser[CreateScope] = "CreateScope("~>stringLiteral~","~statement<~")" ^^ {
    case str~","~sta =>
      val identifier = _obscuringCheck(_stripQuotes(str))
      reservedKeywords.addOne(identifier)
      CreateScope(identifier, sta)
    case _ => throw new Exception("Error parsing!")
  }
  private def createClass: Parser[CreateClass] =
    "CreateClass("~>stringLiteral~opt(","~_getClss)~opt(","~"List("~repsep(createClass, ",")~")")<~")" ^^ {
      case str~None~None => CreateClass(_stripQuotes(str))
      case str~Some(","~clss)~None => CreateClass(_stripQuotes(str), Option(clss))
      case str~None~Some(","~"List("~ls~")") => CreateClass(_stripQuotes(str), toBeNestedClasses = ls)
      case str~Some(","~clss)~Some(","~"List("~ls~")") => CreateClass(_stripQuotes(str), Option(clss), toBeNestedClasses = ls)
      case _ => throw new Exception("Error parsing!")
    }
  private def _getClss: Parser[Class] = "Class("~>stringLiteral<~")" ^^ (
    str => findClass(currentScope, _stripQuotes(str))
  )
  private def declareClassVars: Parser[DeclareClassVars] =
    "DeclareClassVars("~>stringLiteral~","~"List("~repsep(_stringTypeTup, ",")~")"<~")" ^^ {
      case str~","~"List("~ls~")" => DeclareClassVars(_stripQuotes(str), ls)
      case _ => throw new Exception("Error parsing!")
    }
  private def _stringTypeTup: Parser[(String, Type)] = "("~>stringLiteral~","~typ<~")" ^^ {
    case str~","~t => (_stripQuotes(str), t)
    case _ => throw new Exception("Error parsing!")
  }
  private def createClassMethod: Parser[CreateClassMethod] = "CreateClassMethod("~>_getClss~","~stringLiteral~
    opt(","~"List("~repsep(_stringTypeTup, ",")~")")~
    opt(","~"List("~repsep(token, ",")~")")<~")" ^^ {
    case clss~","~methodName~None~None => CreateClassMethod(clss, _stripQuotes(methodName))
    case clss~","~methodName~Some(_~_~paramLs~_)~None => CreateClassMethod(clss, _stripQuotes(methodName), paramLs)
    case clss~","~methodName~None~Some(_~_~tokLs~_) => CreateClassMethod(clss, _stripQuotes(methodName), codes = tokLs)
    case clss~","~methodName~Some(_~_~paramLs~_)~Some(_~_~tokLs~_) =>
      CreateClassMethod(clss, _stripQuotes(methodName), paramLs, tokLs)
    case _ => throw new Exception("Error parsing!")
  }
  private def addNestedClass(): Parser[AddNestedClass] = "AddNestedClass("~>stringLiteral~","~createClass<~")" ^^ {
    case str~","~cc => AddNestedClass(_stripQuotes(str), cc)
    case _ => throw new Exception("Error parsing!")
  }

  private def _obscuringCheck(name: String): String = {
    if(reservedKeywords.contains(name)) throw new Exception(s"Obscuring occurs for '$name'")
    if(name.startsWith("Anon")) throw new Exception(s"Identifier cannot start with 'Anon': $name")
    name
  }
  private def _stripQuotes(nameWithQuotes: String): String = {
    nameWithQuotes.stripSuffix("\"").stripPrefix("\"")
  }
}