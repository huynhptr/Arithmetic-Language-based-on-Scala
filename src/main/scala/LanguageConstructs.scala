import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/*
A scope has a name(an anonymous name is randomly generated for anonymous scope),
a varsTable to store accessible variables within it,
a reference to its parent scope,
and a list of its children scopes.
 */
class Scope(val name: String, val varsTable: mutable.Map[String, Value],
            val parentScope: Option[Scope], val childrenScope: ArrayBuffer[Scope] = ArrayBuffer.empty,
            val classes: ArrayBuffer[Class] = ArrayBuffer.empty)

/*
The blueprint of class instances
 */
class Class(val name: String,
            val superClass: Option[Class] = None,
            var outerClass: Option[Class] = None, //nested classes are created before the outer class, so outerClass needs reassigning
            val classVars: mutable.Map[String, Type] = mutable.Map.empty,
            val nestedClasses: List[Class] = List.empty,
            val methods: ArrayBuffer[Method] = ArrayBuffer.empty
           )

/*
Objects returned by calling myLanguage.createNewInstance().
Require passing Value(s) as argument for constructor to initialize class fields.
 */
class ClassInstance(ofClass: Class, constructorArgs: List[Value]){
  val className: String = ofClass.name
  val superClass: Option[Class] = ofClass.superClass
  val instanceVars: mutable.Map[String, Value] = initInstanceVars
  val methods: ArrayBuffer[Method] = ofClass.methods

  private def initInstanceVars: mutable.Map[String, Value] = {
    val result: mutable.Map[String, Value] = mutable.Map.empty
    //make sure number of passed args equal number of fields
    if(constructorArgs.length != ofClass.classVars.size)
      throw new Exception(s"Expect ${ofClass.classVars.size} arguments for constructor, found ${constructorArgs.length}")
    else {
      //Type checking and initialize class fields
      for((value, classVar) <- constructorArgs.zip(ofClass.classVars)) {
          if(value.t.typeName == classVar._2.typeName)
            result.addOne(classVar._1 -> value)
          else
            throw new Exception(s"Type mismatch in class ${ofClass.name} " +
              s"instantiation: require ${classVar._2.typeName}, found ${value.t.typeName}")
      }
    }
    result
  }

  /*Invoke a method on class instance. There are 4 main steps:
  Step 1: Create a new scope.
  Step 2: Run method code in this scope.
  Step 3: exit to previous scope.
  Step 4: Return the result of the last line of code of the method.
  */
  def InvokeMethod(methodName: String, passedArgs: List[(String, Value)] = List.empty): Option[Value] =
    findMethod(ofClass, methodName) match {
      case Some(method) =>
        parametersCheck(passedArgs, method) //throw exception if there is type mismatch, continue otherwise.
        import Environment.currentScope
        val methodScope = new Scope(s"${ofClass.name}$$$methodName",
          mutable.Map.from(instanceVars ++ passedArgs.toMap), Option(currentScope))
        val previousScope = currentScope
        currentScope = methodScope
        val returned = method.invoke()
        currentScope = previousScope
        returned
      case None => throw new Exception(s"No method $methodName found for class ${ofClass.name} or its superclass")
    }

  //Check number of parameters and type.
  private def parametersCheck(passed: List[(String, Value)], method: Method): Unit =
    if(passed.length != method.params.size)
      throw new Exception(s"Method ${method.name} expects ${method.params.size} arguments, found ${passed.length}")
    else {
      for((name, value) <- passed){
        method.params.find(_._1 == name) match {
          case Some((_, typ)) =>
            if(typ.typeName != value.t.typeName)
              throw new Exception(s"Type mismatch for parameter $name in method ${method.name}" +
                s" of class ${method.ofClass.name}: require ${typ.typeName}, found ${value.t.typeName}")
          case None => throw new Exception(s"Parameter $name is not found in method ${method.name}'s signature!'")
        }
      }
    }

  /*
  Find method upward the class hierarchy first.
  If method is not found, find it outward from nested class to top level class.
   */
  private def findMethod(clss: Class, methodName: String): Option[Method] = {
    @tailrec
    def fromSuper(clssOp: Option[Class]): Option[Method] = {
      clssOp match {
        case None => None
        case Some(clss) => clss.methods.find(_.name == methodName) match {
          case None => fromSuper(clss.superClass)
          case s @ Some(_) => s
        }
      }
    }

    @tailrec
    def fromOuter(clssOp: Option[Class]): Option[Method] = {
      clssOp match {
        case None => None
        case Some(clss) => clss.methods.find(_.name == methodName) match {
          case None => fromOuter(clss.outerClass)
          case s @ Some(_) => s
        }
      }
    }

    clss.methods.find(_.name == methodName) match {
      case option @ Some(_) => option
      case None => fromSuper(Option(clss)) match {
        case s @ Some(_) => s
        case None => fromOuter(Option(clss))
      }
    }
  }
}

class Method(val name: String,
             val params: mutable.Map[String, Type],
             val ofClass: Class,
             val codes: List[Token],
             val lang: ArithmeticLanguage //to reference run() function
            ){

  //An empty function would return None
  def invoke(): Option[Value] = {
    var returned: Option[Value] = None
    for(token <- codes){
      returned = lang.run(token)
    }
    returned
  }

  //format method output
  override def toString: String = {
    val signature = s"$name(ofClass(${ofClass.name}), parameters: $params) = \n"
    val body = codes.map(t => t.toString).mkString("\n")
    signature + "{\n" + body + "\n}"
  }
}