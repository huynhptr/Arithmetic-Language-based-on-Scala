# Peter_Huynh_cs476_hw2 (Arithmetic Language v2)
## Overview
This is an update of Arithmetic Language that incorporates class, inheritance, method, and dynamic dispatch of method invocation. In addition to type 'int', type 'string' is added, so the language now has mechanisms that involve type such as overloaded operation(i.e + and *) and type checking on all variables.

Scala parser combinators are added, this parser is more versatile compare to Regex in version 1, and it makes easier to reason about correctness of syntax.

Several test cases are added to test new functionalities of the language.
## Semantics
Please prefer to README_HW1.md for more semantics information of version 1.

For version 2 semantics, please prefer to source code.
## Sample run
Please prefer to Main.scala for sample run.
## Run instructions
Navigate to the project folder using the command line. Enter ```sbt clean compile run``` to run the program. Enter ```sbt clean compile test``` to run the test cases.
## Comments
- Although there are test cases, it is far from enough.
- There are many great features that come to my mind while I was working on this, but I did not implement them due to time constraint.
- There are many ways to break the code because some important features are not implemented.
- Nested classes are accessible from the global scope. This is more like a drawback than feature. The reason behind is that Arithmetic does not have such thing as "fully qualified name" as in Java, so we cannot refer to nested class by the outer class name.
- Although a nested class can invoke a method that is not its member but is of the outer class, the nested class cannot access fields(class variables) of the outer class because these fields possibly have not been initialized.
- Accessibility of variable is not implemented. That leads to missing other great features.
- I will stop here because it is likely an infinite list of drawbacks.