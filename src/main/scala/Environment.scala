import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/*
The environment of this language.
This is a center of global variables. All language constructs access this singleton object.
 */
object Environment {
  //List of reserved keyword. If any user-defined identifier matches any keyword in this list, obscuring occurs
  val reservedKeywords: mutable.Set[String] = mutable.Set[String]("Token", "Expression", "Statement", "Value", "Add", "Multiply",
    "Variable", "Macro", "Let", "In", "Assign", "Define", "Create", "Scope", "global")

  /*Integer result counter, to imitate Scala REPL, as following Scala code:
  scala> 1 + 2 //hit enter
  val res0: Int = 3
  */
  var resCounter = 0

  //Any macros defined will be stored here, and they are in global scope
  val macrosTable: mutable.Map[String, Expression] = mutable.Map.empty

  /*Set the current scope to global, and predefine some variables.
  Parent scope of global is null.
   */
  var currentScope: Scope = new Scope("global", mutable.Map("peter"->Value(3, Type("int")),
    "zach"->Value(7, Type("int")),
    "var"->Value(2, Type("int"))), null, ArrayBuffer.empty)

  //find Class from the current scope outward to the global scope
  @tailrec
  def findClass(scope: Scope, className: String): Class =
    scope.classes.find(_.name == className) match {
      case Some(clss) => clss
      case None if scope.name != "global" => findClass(scope.parentScope.get, className)
      case _ => throw new Exception(s"Class $className is not found!")
    }

  /*
  If obscuring occur for scope name, class name, or any keywords, an exception is thrown.
   */
  def obscuringCheck(name: String): Unit = {
    if(reservedKeywords.contains(name)) throw new Exception(s"Obscuring occurs for $name")
  }
}
