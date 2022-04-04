import org.scalatest.flatspec.AnyFlatSpec
import Environment._

class ArithmeticSpec extends AnyFlatSpec{
  val myTestLang = new ArithmeticLanguage
  val myParser = new ArithParser

  "Add(Value(1, Type(\"int\")), Value(2, Type(\"int\")))" must "return 3" in {
    assert(myTestLang.eval(
      Add(Value(1, Type("int")), Value(2, Type("int")))) == Value(3, Type("int")))
  }

  "Multiply(Value(2, Type(\"int\")), Value(2, Type(\"int\")))" must "return 4" in {
    assert(myTestLang.eval(
      Multiply(Value(2, Type("int")), Value(2, Type("int")))) == Value(4, Type("int")))
  }

  "DefineMacro(\"aMacro\", Multiply(Add(Variable(\"var\"), Value(1, Type(\"int\"))), Value(3, Type(\"int\")))" must
                                    "add the macro to the global macros list" in {
    myTestLang.execute(DefineMacro("aMacro", Multiply(Add(Variable("var"), Value(1, Type("int"))), Value(3, Type("int")))))
    val Some(mac) = macrosTable.get("aMacro")
    assert(mac ==  Multiply(Add(Variable("var"), Value(1, Type("int"))), Value(3, Type("int"))))
  }

  it must "evaluate to 9 given var=2" in {
    val Some(mac) = macrosTable.get("aMacro")
    assert(myTestLang.eval(
      mac) == Value(9, Type("int")))
  }

  "Assign(\"deadbeef\", Value(3, Type(\"int\")))" must "add the variable to current scope" in {
    myTestLang.execute(Assign("deadbeef", Value(3, Type("int"))))
    val Some(deadbeef) = currentScope.varsTable.get("deadbeef")
    assert(deadbeef == Value(3, Type("int")))
  }

  it must "return 3 when Variable(\"deadbeef\") is evaluated" in {
    assert(myTestLang.eval(
      Variable("deadbeef")) == Value(3, Type("int")))
  }

  "CreateScope(\"scope1\",Assign(\"scopeVar\",Value(11, Type(\"int\"))))" must "update the current scope to 'scope1'" in {
    myTestLang.execute(CreateScope("scope1",Assign("scopeVar",Value(11, Type("int")))))
    assert(currentScope.name == "scope1")
  }

  it must "have global as its parent scope" in {
    assert(currentScope.parentScope.get.name == "global")
  }

  it must "be a child scope of global" in {
    assert(currentScope.parentScope.get.childrenScope.contains(currentScope))
  }

  "Let(Assign(\"var2\", Value(9, Type(\"int\"))) In Add(Variable(\"var2\"), Value(1, Type(\"int\")))" must "be parsed correctly" in {
    assert(myParser.tokenize("Let(Assign(\"var2\", Value(9, Type(\"int\")))) In Add(Variable(\"var2\"), Value(1, Type(\"int\")))").head
      == Let(Assign("var2", Value(9, Type("int"))), Add(Variable("var2"), Value(1, Type("int")))))
  }

  it must "create an anonymous scope and exit this scope when done the evaluation" in {
    val parsedLet = myParser.tokenize("Let(Assign(\"var2\", Value(9, Type(\"int\")))) In Add(Variable(\"var2\"), Value(1, Type(\"int\")))")
    myTestLang.run(parsedLet.head)
    val anonScope = currentScope.childrenScope(0)

    assert(anonScope.name.startsWith("Anon$"))
    assert(currentScope == anonScope.parentScope.get)
  }

  it must "evaluate to 10" in {
    val parsedLet = myParser.tokenize("Let(Assign(\"var2\", Value(9, Type(\"int\")))) In Add(Variable(\"var2\"), Value(1, Type(\"int\")))").head
    assert(myTestLang.eval(
      parsedLet.asInstanceOf[Expression]) == Value(10, Type("int")))
  }

  "Assign(\"x\", Value(7, Type(\"int\")))\nAssign(\"x\", Value(0, Type(\"int\")))\nVariable(\"x\")"must
    "have x=0 because x=7 is shadowed in the same scope" in {
    myTestLang.execute(Assign("x",Value(7, Type("int"))))
    myTestLang.execute(Assign("x", Value(0, Type("int"))))
    assert(myTestLang.eval(
      Variable("x")) == Value(0, Type("int")))
  }

  "Assign(\"x\", Value(7, Type(\"int\")))\nCreate(\"shadowScope\",Assign(\"x\", Value(0, Type(\"int\"))))\nVariable(\"x\")"must
    "have x=0 because x=7 in global is shadowed by x in shadowScope" in {
    myTestLang.execute(Assign("x",Value(7, Type("int"))))
    myTestLang.execute(CreateScope("shadowScope",Assign("x", Value(0, Type("int")))))
    assert(myTestLang.eval(
      Variable("x")) == Value(0, Type("int")))
  }

  "DefineMacro(\"Macro\", Multiply(Variable(\"peter\"), Value(2, Type(\"int\"))))" must
    "throw an Exception because the reserved keyword 'Macro' is obscured" in {
    val thrown = intercept[Exception](
      myParser.tokenize("DefineMacro(\"Macro\", Multiply(Variable(\"peter\"), Value(2, Type(\"int\"))))"))
    assert(thrown.getMessage == "Obscuring occurs for 'Macro'")
  }

  "CreateScope(\"scope1\", DefineMacro(\"Macro\", Multiply(Variable(\"peter\"), Value(2, Type(\"int\")))))\nAssign(\"scope1\", Value(7, Type(\"int\")))" must
    "throw an Exception because the scope name 'scope1' is obscured" in {
    myTestLang.execute(CreateScope("scope1", DefineMacro("Macro", Multiply(Variable("peter"), Value(2, Type("int"))))))
    val thrown = intercept[Exception](myParser.tokenize("Assign(\"scope1\", Value(7, Type(\"int\")))"))
    assert(thrown.getMessage == "Obscuring occurs for 'scope1'")
  }

  "Assign(\"Anonocha\", Value(12, Type(\"int\")))" must "throw an Exception because name started with 'Anon' is illegal" in {
    val thrown = intercept[Exception](myParser.tokenize("Assign(\"Anonocha\", Value(12, Type(\"int\")))"))
    assert(thrown.getMessage == "Identifier cannot start with 'Anon': Anonocha")
  }

  /************ HW2 tests **************/

  "CreateClass(\"Base\")" must "create a Class(blueprint) name 'Base'" in {
    val token = myParser.tokenize("CreateClass(\"Base\")").head
    myTestLang.execute(token.asInstanceOf[Statement])
    assert(findClass(currentScope, "Base").name == "Base")
  }

  "DeclareClassVars(\"Base\", List((\"baseVar\", Type(\"int\"))))" must "create fields for Class Base" in {
    val token = myParser.tokenize("DeclareClassVars(\"Base\", List((\"baseVar\", Type(\"int\"))))").head
    myTestLang.execute(token.asInstanceOf[Statement])
    assert(findClass(currentScope, "Base").classVars.head == ("baseVar", Type("int")))
  }

  "CreateClassMethod(Class\"Base\"), \"m1\", List(), List(Value(\"Method m1 of Base is called!\", Type(\"string\"))))" must
    "create method 'm1' for Class 'Base'" in {
    val token = myParser.tokenize("CreateClassMethod(Class(\"Base\"), \"m1\", List(), List(Value(\"Method m1 of Base is called!\", Type(\"string\"))))").head
    myTestLang.execute(token.asInstanceOf[Statement])
    assert(findClass(currentScope, "Base").methods.head.name == "m1")
  }

  "CreateClass(\"Derived\", Class(\"Base\"))" must "create Class Derived as a subclass of Base" in {
    val token = myParser.tokenize("CreateClass(\"Derived\", Class(\"Base\"))").head
    myTestLang.execute(token.asInstanceOf[Statement])
    assert(findClass(currentScope, "Derived").superClass.get.name == "Base")
  }

  """DeclareClassVars("Derived", List(("derivedIntVar", Type("int")), ("derivedStringVar", Type("string"))))""" must
  "create 2 fields named \"derivedIntVar\" and \"derivedStringVar\" for Class Derived" in {
    val token = myParser.tokenize("""DeclareClassVars("Derived", List(("derivedIntVar", Type("int")), ("derivedStringVar", Type("string"))))""").head
    myTestLang.execute(token.asInstanceOf[Statement])
    assert(findClass(currentScope, "Derived").classVars.contains("derivedIntVar") &&
      findClass(currentScope, "Derived").classVars.contains("derivedStringVar"))
  }

  it must "shows Class Derived has 3 class variables(1 inherited and 2 non-inherited)" in {
    assert(findClass(currentScope, "Derived").classVars.size == 3)
  }

  val createMethodCode: String =
     """|CreateClassMethod(Class("Derived"), "m2", List(("p1", Type("int")), ("p2", Type("string"))),
        |  List(
        |    Assign("m2Var1", Add(Variable("var"), Macro("mac1"))),
        |    Assign("m2Var3",
        |      Let(Assign("m2Var2", Add(Variable("baseVar"), Variable("derivedIntVar")))) In Add(Variable("m2Var2"), Variable("m2Var1"))),
        |    Assign("greeting", Add(Variable("p2"), Variable("derivedStringVar"))),
        |    Multiply(Variable("greeting"), Add(Add(Variable("p1"), Variable("m2Var3")), Value(-10, Type("int"))))
        |  )
        |)
        |""".stripMargin

  s"Parse and run: \n$createMethodCode" must "create method 'm2' for Class Derived" in {
    val token = myParser.tokenize(createMethodCode).head
    myTestLang.execute(token.asInstanceOf[Statement])
    assert(findClass(currentScope, "Derived").methods.head.name == "m2")
  }

  "Method m2()" must "received 2 arguments" in {
    assert(findClass(currentScope, "Derived").methods.head.params.size == 2)
  }

  it must "have 4 lines of codes" in {
    assert(findClass(currentScope, "Derived").methods.head.codes.size == 4)
  }

  var derivedInstance: ClassInstance = _
  """mytestLang.createNewInstance(findClass(currentScope,"Derived"),
    |    List(Value(6, Type("int")), Value(0, Type("int")), Value("funnybunny ", Type("string"))))""".stripMargin must
  "create an instance of Class Derived successfully" in {
    derivedInstance = myTestLang.createNewInstance(findClass(currentScope,"Derived"),
      List(Value(6, Type("int")), Value(0, Type("int")), Value("funnybunny ", Type("string"))))
    assert(derivedInstance.isInstanceOf[ClassInstance])
  }

  it must "properly initialize this instance's fields" in {
    assert(derivedInstance.instanceVars.values.toList == List(Value(6, Type("int")), Value(0, Type("int")), Value("funnybunny ", Type("string"))))
  }

  """derivedInstance.InvokeMethod("m2", List(("p1", Value(2, Type("int"))), ("p2", Value("howdy!", Type("string")))))""" must
  "return 3 times of the string 'howdy!funnybunny '" in {
    //create macro 'mac1'
    myTestLang.execute(DefineMacro("mac1", Multiply(Add(Variable("var"), Value(1, Type("int"))), Value(1, Type("int")))))
    val m2Result = derivedInstance.InvokeMethod("m2", List(("p1", Value(2, Type("int"))), ("p2", Value("howdy!", Type("string")))))
    assert(m2Result.get == Value("howdy!funnybunny howdy!funnybunny howdy!funnybunny ", Type("string")))
  }

  """derivedInstance.InvokeMethod("m1")""" must "return the string 'Method m1 of Base is called!'" in {
    val m1Result = derivedInstance.InvokeMethod("m1")
    assert(m1Result.get == Value("Method m1 of Base is called!", Type("string")))
  }

  """AddNestedClass("Base", CreateClass("Nested"))""" must "create a nested class Nested in Base" in {
    val token = myParser.tokenize("""AddNestedClass("Base", CreateClass("Nested"))""").head
    myTestLang.execute(token.asInstanceOf[Statement])
    assert(findClass(currentScope, "Nested").outerClass.get.name == "Base")
  }

  """AddNestedClass("Derived", CreateClass("Inner1st", List(CreateClass("Inner2nd"))))""" must
  "create Class Inner2nd nested in Class Inner1st, and Class Inner1st nested in Derived" in {
    val token = myParser.tokenize("""AddNestedClass("Derived", CreateClass("Inner1st", List(CreateClass("Inner2nd"))))""").head
    myTestLang.execute(token.asInstanceOf[Statement])
    assert(findClass(currentScope, "Inner2nd").outerClass.get.name == "Inner1st")
    assert(findClass(currentScope, "Inner1st").outerClass.get.name == "Derived")
  }

  """DeclareClassVars("Nested", List(("nestedVar", Type("string"))))""" must
  "shows Nested have 1 field since Nested does not inherit any field from outer class Base" in {
    val token = myParser.tokenize("""DeclareClassVars("Nested", List(("nestedVar", Type("string"))))""").head
    myTestLang.execute(token.asInstanceOf[Statement])
    assert(findClass(currentScope, "Nested").classVars.size == 1)
  }

  """nestedInstance.InvokeMethod("m1")""" must "be able to invoke outer class method m1()" in {
    val nestedInstance = myTestLang.createNewInstance(findClass(currentScope, "Nested"), List(Value("I am in Nested!", Type("string"))))
    val result = nestedInstance.InvokeMethod("m1")
    assert(result.get == Value("Method m1 of Base is called!", Type("string")))
  }

}
