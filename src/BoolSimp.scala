//package hw3
//import hw3.ExpressionTree

object BoolSimp {

  val DEBUG = true;

    val introString = """
   --------------------------------------------------------------------------
 /--------------------------------- BoolSimp --------------------------------\
|------------------------------------------------------------------------------|
| Welcome to BoolSimp!                                                         |
|                                                                              |
| BoolSimp takes a boolean expression and some bindings and produces           |
| a simplified expression with the given bindings.                             |
|                                                                              |
| For example, consider the following boolean expression:                      |
|   (or x 0)                                                                   |
|   This expression simplifies to 'x'.                                         |
| Suppose 'x' is bound to 1. Note that 'bound to' equates to 'replaced with'.  |
| Then the previous expression becomes:                                        |
|   (or 1 0)                                                                   |
|   ...which simplifies to 1.                                                  |
|                                                                              |
| * Note that 'x' could have been bound to, say, 'y'. This would have changed  |
|   the expression to (or y 0) and the simplificiation to 'y'.                 |
|                                                                              |
| ** To access the general help menu, type (help).                             |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val helpString = """
   --------------------------------------------------------------------------
 /------------------------------- General Help ------------------------------\
|------------------------------------------------------------------------------|
| General Command Syntax:                                                      |
|   (keyword argument1 argument2)                                              |
|                                                                              |
| Valid Keywords Include:                                                      |
|   quit                                                                       |
|   help                                                                       |
|   evalxp                                                                     |
|                                                                              |
| Valid Arguments Include:                                                     |
|   Any Keyword                                                                |
|   Expressions                                                                |
|   Binding Lists                                                              |
|                                                                              |
| Example Commands:                                                            |
|   (quit)                                  --> Exits BoolSimp                 |
|   (help)                                  --> Displays this help menu        |
|   (help evalxp)                           --> Displays the evalxp help menu  |
|   (evalxp (and y 1))                      --> Returns 'Y'                    |
|   (evalxp (or x 1) ((x 0)))               --> Returns '1'                    |
|   (evalxp (not (and a b)) ((a x) (b y)))  --> Returns '1'                    |
|                                                                              |
| *** One of the most common mistakes is not putting the correct number of     |
|     parentheses at the end of each command. All open parentheses must have a |
|     matching closing parenthesis.                                            |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val helpEvalxp = """
   --------------------------------------------------------------------------
 /------------------------------ Help: 'evalxp' ------------------------------\
|------------------------------------------------------------------------------|
| Command Syntax:                                                              |
|   (evalxp boolean-expression optional-binding-list)                          |
|                                                                              |
| Valid Boolean Expressions Include:                                           |
|   (and a1 a2)                                                                |
|   (or a1 a2)                                                                 |
|   (not a1)                                                                   |
|                                                                              |
| Binding List Syntax:                                                         |
|   ((var1 bind1) (var2 bind2) (var3 bind3) (var4 bind4) ...)                  |
|   ...where var1, var2, var3, etc. are variables that appear in an expression |
|      and bind1, bind2, bind3, etc. are strings for the variables to be bound |
|      to.                                                                     |
|                                                                              |
| Example Command:                                                             |
|   (evalxp (or x (and y z)) ((z 1) (x csc344) (y 0)))    --> Returns 'CSC344' |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val helpHelp = """
   --------------------------------------------------------------------------
 /------------------------------- Help: 'help'--------------------------------\
|------------------------------------------------------------------------------|
| This is just silly. If you got here, you already know the syntax for the     |
| 'help' command :P.                                                           |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val helpQuit = """
   --------------------------------------------------------------------------
 /------------------------------- Help: 'quit' -------------------------------\
|------------------------------------------------------------------------------|
| Really?! You need help quitting this application??                           |
| (quit) was the first example given in the 'General Help' section.            |
| Try taking a look there. All you have to do is enter (help) as a command.    |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val commandEntryError = """
   --------------------------------------------------------------------------
 /--------------------------------- uh oh... ---------------------------------\
|------------------------------------------------------------------------------|
| The command you have entered is invalid. Please refer to the help manual     |
| for information about how to properly use this utility.                      |
|                                                                              |
| (help) will bring you to the help manual...                                  |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val noListInputError = """
   --------------------------------------------------------------------------
 /--------------------------------- oops... ----------------------------------\
|------------------------------------------------------------------------------|
| All commands must be enclosed by parentheses.                                |
|                                                                              |
| Type '(help)' (without the quotes) for further assistance.                   |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val thanks = "\n--- Thank you for using BoolSimp! ---\n\n"

  val p1S = List("and", 'x', List("or", 'x', List("and", 'y', List("not", 'z'))))
  val p1 = new ExpressionTree("and", null, null)

  def stringToExpressionTree(expressionString: String): ExpressionTree = {
    if (DEBUG) {
      println(" ---------------------------------------------------------- ")
      println("| Beginning call to stringToExpressionTree                 |")
      println("|----------------------------------------------------------|")
      println("| Expression String: " + "%-37s |".format(expressionString))
    }
    //Trim the expressionString
    val trimmedExpressionString = expressionString.trim
    if (DEBUG) {
      println("| Trimmed Version: " + "%-39s |".format(trimmedExpressionString))
    }
    if (trimmedExpressionString(0) != '(' || trimmedExpressionString(trimmedExpressionString.length-1) != ')') {
      println("|..........................................................|")
      println("| That is not a valid expression. All expressions must     |")
      println("| be enclosed by parenthesis.                              |")
      println(" ---------------------------------------------------------- ")
    }
    val expressionTree = new ExpressionTree("and", null, null)
    return expressionTree
  }

  def main(args: Array[String]) {
    val test = new ExpressionTree("string", null, null)
    println(test.root)
    println(introString)
    //println(p1.root)
    stringToExpressionTree("   SomeString)")
    /*
    println("p1 = " + p1)
    println("head = " + p1.head)
    println("p1.init = " + p1.init)
    println("p1.isEmpty = " + p1.isEmpty)
    println("p1.last = " + p1.last)
    println("p1.last.getClass() = " + p1.last.getClass())
    println("List.getClass() = " + List.getClass())
    val p1List = p1.last.asInstanceOf[List[Any]]
    println("p1List = " + p1List)
    println("p1List.getClass() = " + p1List.getClass())
    println("p1.length = " + p1.length)
    println("p1.reverse = " + p1.reverse)
    println("p1.tail = " + p1.tail)
    println("p1.tail.tail = " + p1.tail.tail)
    println("p1.tail.tail.head = " + p1.tail.tail.head)
    */
  }
}
