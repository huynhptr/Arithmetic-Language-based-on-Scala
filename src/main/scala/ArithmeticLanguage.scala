import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import Environment._

class ArithmeticLanguage {

  /*Run a Token and print formatted outputs.
  Return a Value if an Expression is run.
  Return None otherwise(if a Statement is run).
   */
  def run(token: Token): Option[Value] = {
    /*Helper function to figure out indentation for scopes outputting.
    Deeper nested scope is printed with more indents*/
    def getIndentation: String = {
      @tailrec
      def helper(scopeRef: Scope, soFar: String) : String = {
        if (scopeRef.name == "global") soFar
        else helper(scopeRef.parentScope.get, "  ")
      }
      val scope = currentScope
      helper(scope, "")
    }

    if (token.tokenType == "Expression") {
      val res = eval(token.asInstanceOf[Expression])
      println(s"${getIndentation}res$resCounter: Int = $res")
      resCounter += 1

      //If Let() In Expression was run, exit current anonymous scope, go back to outer(parent) scope
      if (currentScope.name.startsWith("Anon"))
        currentScope = currentScope.parentScope.get

      return Option(res)
    }

    val resString = execute(token.asInstanceOf[Statement])
    if(resString != "") println(resString)

    None
  }

  //Evaluate an Expression and output a Value as a result.
  def eval(expression: Expression) : Value = expression match {
    case v @ Value(_, _) => v
    case Add(op1, op2) => eval(op1) + eval(op2)
    case Multiply(op1, op2) => eval(op1) * eval(op2)
    case Variable(name) => findVariableScopewise(currentScope, name)
    case Macro(name) => eval(macrosTable.getOrElse(name, throw new Exception(s"Undefined macro: $name")))

    /*Let() In Expression involves a Statement and an Expression.
    A new anonymous scope is created, then the Statement is executed, then the Expression is evaluated in this new scope
     */
    case Let(statement, expression) =>
      val newScope = createNewScope(anonGenerator())
      val tailCallResult = for(line <- execute(statement).split("\n")) yield "  " + line
      println(s"In ${newScope.name}: Scope => \n${tailCallResult.mkString("\n")}")
      eval(expression)

  }

  //Find variable starting the current scope outward to the global scope
  def findVariableScopewise(scope: Scope, name: String): Value =
    scope.varsTable.getOrElse(name,
      scope.parentScope match {
        case Some(s) => findVariableScopewise(s, name)
        case None => throw new Exception(s"Undefined variable: $name")
      }
    )

  //Create a new scope given a name, and set the current scope to this new scope.
  def createNewScope(name: String): Scope = {
    val newScope = new Scope(name, mutable.Map.empty, Option(currentScope), ArrayBuffer.empty)
    reservedKeywords.addOne(name)
    currentScope.childrenScope.append(newScope)
    currentScope = newScope
    currentScope
  }

  //Generate random scope name
  @tailrec
  private def anonGenerator(): String = {
    val newAnon = "Anon$" + new Random().nextInt(100)
    if(reservedKeywords.contains(newAnon)) anonGenerator()
    else newAnon
  }

  //Execute a Statement and output a string describing what the Statement did.
  def execute(statement: Statement): String = statement match {
    case Assign(varName, expression) =>
      val value = eval(expression)
      currentScope.varsTable += (varName->value)
      s"$varName: Int = $value"

    case DefineMacro(macroName, expression) =>
      macrosTable += (macroName->expression)
      s"$macroName: Macro = ${expression.toString}"

    case CreateScope(scopeName, statement) =>
      createNewScope(scopeName)
      val tailCallResult = for(line <- execute(statement).split("\n")) yield "  " + line
      s"In $scopeName: Scope => \n${tailCallResult.mkString("\n")}"

    case c @ CreateClass(_, _, _) =>
      currentScope.classes.addOne(createClass(c))
      "" //output string is already produced by createClass()

    case DeclareClassVars(className, vars) =>
      val classRef = findClass(currentScope, className)
      classRef.classVars.addAll(vars)
      s"class $className variables: ${classRef.classVars}"

    case CreateClassMethod(ofClass, methodName, params, codes) =>
      val newMethod = new Method(methodName, mutable.Map.from(params), ofClass, codes, this)
      ofClass.methods.addOne(newMethod)
      s"class ${ofClass.name} method $newMethod"

    case AddNestedClass(existingClassName, toBeCreated) =>
      val existingClass = findClass(currentScope, existingClassName)
      val nested = createClass(toBeCreated)
      nested.outerClass = Option(existingClass)
      //noinspection ScalaUnusedExpression
      existingClass.nestedClasses :+ nested
      s"Nested class ${toBeCreated.className} is added to class $existingClassName"
  }

  /*
  For consistency, there should be a case class CreateNewInstance extends Statement so it can be executed.
  However, this is not a case class because of the need to invoke it in Scala, not Arithmetic Language.
  So I made it a function.
  Sample usage: val instance = createNewInstance(Class("Derived))
  */
  def createNewInstance(ofClass: Class, constructorArgs: List[Value]): ClassInstance = {
    val ins = new ClassInstance(ofClass, constructorArgs)
    println(s"An instance of class ${ofClass.name} is created.")
    ins
  }

  def createClass(cc: CreateClass): Class = {
    obscuringCheck(cc.className)

    //create nested classes
    val nested = for(c <- cc.toBeNestedClasses) yield createClass(c)
    //get inherited variables from super class
    val inheritedVars: mutable.Map[String, Type] = cc.superClass match {
      case Some(sup) => sup.classVars
      case None => mutable.Map.empty
    }
    //create the new class
    val newClass = new Class(cc.className, cc.superClass, nestedClasses = nested, classVars = inheritedVars)
    reservedKeywords.addOne(newClass.name)
    currentScope.classes.addOne(newClass) //all classes are visible in current scope, including nested class
    //update nested classes' outerClass to this new class
    newClass.nestedClasses.foreach(_.outerClass = Option(newClass))
    //construct and print output
    println(s"class ${cc.className}" + (if(cc.superClass.isDefined) s" extends ${cc.superClass.get.name}" else "") +
      (
        if(nested.nonEmpty)
          " with nested class(es): " + nested.map(_.name).mkString(",")
        else ""
        ))
    newClass
  }
}
