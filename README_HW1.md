# Peter_Huynh_cs476_hw1
## Overview
This program simulates an integer arithmetic language that includes addition and multiplication. This language also has characteristics such as variables, macros, and scopes. There is a parser that processes textual code into executable tokens. This parser acts similarly to a compiler in common programming languages. Each line of textual code is parsed to one token(possibly with nested tokens). These resulting tokens will then execute sequentially as in the order of the textual code. During parsing and execution, these tokens will update the environment accordingly. Environment consists of the list of reserved keywords, a table to store macros, current executing scope, and a result counter to imitate Scala REPL outputs.

## Semantics
**Token:** includes two subtypes are Expression and Statement. A token is a unit of code that corresponds to one textual line of code.

**Expresion:** can be recursively evaluated to an integer result. Expression includes the following operations:

- **Value:** is the basic unit of this language. It is a wrapper of an integer, and therefore evaluated to an integer.

- **Add:** takes two Expression arguments, and return the sum of the evaluated results of these Expression.

- **Multiply:** takes two Expression arguments, and return the product of the evaluated results of these Expression.

- **Variable:** takes a name as String, then search the environment variables table from the current scope outward to global scope, and return its value.

- **Macro:** takes a name as String, then search the environment macros table, and return the macro expression.

- **Let:** is the result of parsing "Let(Statement) In (Expression)". It will create a new anonymous scope, then execute the Statement, and then evaluate the Expression in this scope. 
  
Note:
- Accessing undefined variables or macros will throw exceptions.

- Most of these Expressions will not change the environment except Let() because the Statement it executes may change the environment.

**Statement:** is the introduction of new variable, macro, or scope to the environment. Statement includes the following operations:

- **Assign:** takes a String name, and defines a new variable with a given expression. The expression is evaluated before binding to the variable name. This variable is added to the environment variables table.

- **Define:** takes a String name, and defines a new macro with the given expression. This Expression is NOT evaluated until Macro() is called. This macro is added to the environment macros table.

- **Create:** takes a String name, and create a new scope with this name nested in the current scope, then update the current scope to this new scope. It then executes the given Statement in this new scope.

Note:
- Any name that matches any reserved keyword will cause obscuring exception.
- Any name started with "Anon" is prohibited because it will clash with anonymous scope name.
- For Assign() and Define(), the new variable or macro will shadow any in-scope variable or macro with the same name, respectively.

## Sample run

This sample code is very similar to the code in the homework description:

```scala
//define a new macro name 'mac1', and usage of accessing variable 'var'
DefineMacro("mac1", Multiply(Add(Variable("var"), Value(1)), Value(3)))
//assign a new variable name 'var1', and usage of macro 'mac1'
Assign("var1", Add(Variable("var"), Macro("mac1")))
//assign a new variable 'var2' in an anonymous scope, then evaluate the Expression after In
Let(Assign("var2", Add(Variable("var"), Macro("mac1")))) In Add(Variable("var2"), Value(1))
//create 3 nested scope, and assign a new variable in the innermost scope with usage of macro
CreateScope("scope1", CreateScope("scope2", CreateScope("scope3", Assign("var3", Add(Variable("var"), Macro("mac1"))))))

/*
Output: given variable var=4
-> Define("mac1", Multiply(Add(Variable("var"), Value(1)), Value(3)))
mac1: Macro = Multiply(Add(Variable(var),Value(1)),Value(3))
-> Assign("var1", Add(Variable("var"), Macro("mac1")))
var1: Int = 19
-> Let(Assign("var2", Add(Variable("var"), Macro("mac1")))) In Add(Variable("var2"), Value(1))
In Anon$69: Scope => 
  var2: Int = 19
  res0: Int = 20
-> Create("scope1", Create("scope2",  Create("scope3",Assign("var3", Add(Variable("var"), Macro("mac1"))))))
In scope1: Scope => 
  In scope2: Scope => 
    In scope3: Scope => 
      var3: Int = 19
*/
```
Beside the sample code, there are several test cases that simulate different features of this language. These test cases are written using FlatSpec. See instruction below to run them.

## Run instructions
Navigate to the project folder using the command line. Enter ```sbt clean compile run``` to run the program. Enter ```sbt clean compile test``` to run the test cases.

## Future improvements
- This program relies heavily on global and mutable variables. It needs to be refactored to reflect functional style with immutable variables and avoid global variables.
- Functions with side effects need to be eliminated.
- At the moment, only anonymous scope exits when finishing execution all of its tokens. Named scopes will stay in-scope. Mechanisms to exit named scope when done executing, and simulation of holes between same level scopes, are needed in future improvement.