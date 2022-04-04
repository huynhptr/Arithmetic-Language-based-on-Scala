import Environment.findClass
import Environment.currentScope

object Main extends App {
  println("\n******************************HW1 SAMPLE RUN********************************************************\n")
  //One line of code is terminated by a newline if the line is not ended by a comma, or parenthesis
  val codes =
    """|DefineMacro("mac1", Multiply(Add(Variable("var"), Value(1, Type("int"))), Value(1, Type("int"))))
       |Assign("var1", Add(Variable("var"), Macro("mac1")))
       |Let(Assign("var2", Add(Variable("var"), Macro("mac1")))) In Add(Variable("var2"), Value(1, Type("int")))
       |CreateScope("scope1",
       |  CreateScope("scope2",
       |    CreateScope("scope3",
       |      Assign("var3", Add(Variable("var"), Macro("mac1"))))))
       |""".stripMargin

  val myLanguage: ArithmeticLanguage = new ArithmeticLanguage
  val parser: ArithParser = new ArithParser
  parseAndRun(codes)

  println("\n******************************END HW1 SAMPLE RUN, START HW2 SAMPLE RUN******************************\n")
  /*
  Arithmetic programs must be run segment by segment because a Class need to be created before it can be
  retrieved by Class("className")
   */

  /*
  1. Create a Class(blueprint of ClassInstances)
  2. Declare its fields with associating types
   */
  val codes1 =
    """|CreateClass("Base")
       |DeclareClassVars("Base", List(("baseVar", Type("int"))))
       |""".stripMargin
  parseAndRun(codes1)

  /*
  1. Create a method m1 of Class 'Base' that takes no parameter, and return a 'string' Value.
  2. Create a Class 'Derived' which is a subclass of 'Base'
  3. Declare Class 'Derived' fields with associating types
   */
  val codes2 =
    """|CreateClassMethod(Class("Base"), "m1", List(), List(Value("Method m1 of Base is called!", Type("string"))))
       |CreateClass("Derived", Class("Base"))
       |DeclareClassVars("Derived", List(("derivedIntVar", Type("int")), ("derivedStringVar", Type("string"))))
       |""".stripMargin
  parseAndRun(codes2)

  /*
  Create a method m2 of Class 'Derived' that takes 2 arguments of type 'int' and 'string'
  Method m2 body purposes:
    1. Retrieve global variable 'var' and macro 'mac1'
    2. Retrieve inherited variable 'baseVar' from Class 'Base', and non-inherited variable 'derivedIntVar'
    3. Concatenate passed variable 'p2' and non-inherited variable 'derivedStringVar' to make a string
    4. String concatenation using Multiply. Returned result is 3 times of the string 'howdy!funnybunny'
   */
  val codes3 =
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
  parseAndRun(codes3)

  /*
  Create a new instance of Class Derived, and simulate passing argument to constructor.
  The first Value is to initialize inherited variable 'baseVar' from Class Base.
  The latter two Values are to initialize fields 'derivedIntVar' and 'derivedStringVar' of Class Derived.
   */
  val derivedInstance = myLanguage.createNewInstance(findClass(currentScope,"Derived"),
    List(Value(6, Type("int")), Value(0, Type("int")), Value("funnybunny ", Type("string"))))

  /*
  Invoke method of Class Derived, and pass arguments BY NAME.
   */
  val m2result = derivedInstance.InvokeMethod("m2", List(("p1", Value(2, Type("int"))), ("p2", Value("howdy!", Type("string")))))

  /*
  Invoke inherited method "m1" of Class Base from Class Derived instance.
   */
  val m1result = derivedInstance.InvokeMethod("m1")

  /*
  1. Create a new Class Nested and add it as a nested class of Class Base.
  2. Create a new Class Inner1st and add it as a nested class of Class Derived.
     During Inner1st creation, also create a new nested class Inner2nd of Inner1st.
  3. Declare Class Nested fields.
   */
  val codes4 =
    """
      |AddNestedClass("Base", CreateClass("Nested"))
      |AddNestedClass("Derived", CreateClass("Inner1st", List(CreateClass("Inner2nd"))))
      |DeclareClassVars("Nested", List(("nestedVar", Type("string"))))
      |""".stripMargin
  parseAndRun(codes4)

  /*
  Create new instance of Nested and pass argument for constructor
   */
  val nestedInstance = myLanguage.createNewInstance(findClass(currentScope, "Nested"), List(Value("I am in Nested!", Type("string"))))

  /*
  Class Nested neither have or inherit method 'm1', so when invoking 'm1' from its instance, method 'm1' from its
  outer class is invoked.
   */
  val m1FromNested = nestedInstance.InvokeMethod("m1") //invoke outer class method

  //helper function
  def parseAndRun(codes: String): Unit ={
    for(token <- parser.tokenize(codes)){
      println(s"\nArithmetic> $token")
      print("> ")
      myLanguage.run(token)
    }
  }
}
