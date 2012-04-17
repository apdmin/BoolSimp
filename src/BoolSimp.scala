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
    var normalizedES = expressionString.trim
    normalizedES = normalizeExpressionString(normalizedES)
    if (DEBUG) {
      println("| Normalized Version: " + "%-36s |".format(normalizedES))
    }
    if (normalizedES(0) != '(' || normalizedES(normalizedES.length-1) != ')') {
      println("|..........................................................|")
      println("| That is not a valid expression. All expressions must     |")
      println("| be enclosed by parenthesis.                              |")
      println(" ---------------------------------------------------------- ")
    }

    /*
    Algorithm:
      • Parse through the string.
      • If you see an open paren, need to instantiate a new ExpressionTree
    */

    //Can assume the first character is an open paren
    if (normalizedES.charAt(0) != '(')
      throw new Error("First character: '" + normalizedES.charAt(0) + "' was not an open parenthesis")
    //Since the first character is an open paren, we can create an ExpressionTree right away
    //Figure out which expression tree to create
    var expressionTree: ExpressionTree = null
    if (normalizedES.substring(1, 4) == "and") {
      //Extract the first and second parameters
      val firstParam = extractFirstParam(normalizedES)
      val secondParam = extractSecondParam(normalizedES)

      //Check to see if the first parameter is an actual parameter or is another expressionTree
      //    (  a  n  d     (  o  r...
      //    0  1  2  3  4  5  6  7
      //                   ^
      if (normalizedES.charAt(5) == '(') {
        //We have an expressionTree
        //Check the second parameter
      } else if (normalizedES.charAt(6) == ' ') {
        //We have a single character parameter
        //Check the second parameter
      } else {
        throw new Error("First parameter is invalid")
      }
      expressionTree = createAndExpression(normalizedES.substring(5, 6), normalizedES.substring(7, 8))
      println(expressionTree)
    } else if (normalizedES.substring(1, 3) == "or") {
      expressionTree = createOrExpression(normalizedES.substring(4, 5), normalizedES.substring(6, 7))
      println(expressionTree)
    } else if (normalizedES.substring(1, 4) == "not") {
      expressionTree = createNotExpression(normalizedES.substring(5, 6))
      println(expressionTree)
    }
    return expressionTree
  }

  def extractFirstParam(expression: String): String = {
    //We can assume the given expression has already been normalized
    if (expression.substring(1, 4) == "and") {
      var param = ""
      var parenCount = 0
      var index = 5;
      do {
        if (expression.charAt(index) == '(')
          parenCount += 1
        else if (expression.charAt(index) == ')')
          parenCount -= 1
        param = param + expression.charAt(index)
        index += 1
      } while (parenCount > 0)
      return param
    }
    throw new Error ("extractFirstParam encountered an error that prevented it from working")
  }
  def extractSecondParam(expression: String): String = {
    //We can assume the given expression has already been normalized
    return ""
  }


  def createAndExpression(left: String, right: String): ExpressionTree = {
    val l = new ExpressionTree(left, null, null)
    val r = new ExpressionTree(right, null, null)
    new ExpressionTree("and", l, r)
  }

  def createOrExpression(left: String, right: String): ExpressionTree = {
    val l = new ExpressionTree(left, null, null)
    val r = new ExpressionTree(right, null, null)
    new ExpressionTree("or", l, r)
  }

  def createNotExpression(argument: String): ExpressionTree = {
    val arg = new ExpressionTree(argument, null, null)
    new ExpressionTree("not", arg, null)
  }

  def normalizeExpressionString(expression: String): String = {
    var output = ""
    val length = expression.length
    var currentChar = '~'
    var lastChar = '~'
    for (i <- 0 until length) {
      lastChar = currentChar
      currentChar = expression.charAt(i)
      if (lastChar == ' ' && currentChar == ' ') {
      } else if (lastChar == '(' && currentChar == ' ') {
      } else if (lastChar == ' ' && currentChar == ')') {
        output = output.substring(0, (output.length-1))
        output = output + currentChar
      } else if (lastChar != '~' && lastChar != ' ' && currentChar == '(') {
        output = output + ' ' + currentChar
      } else {
        output = output + currentChar
      }
    }
    return output
  }


  def main(args: Array[String]) {
    val andExpression = createAndExpression("1", "0")
    println(introString)
    println(extractFirstParam("(and (or 1 (not 0)) 0)"))
    //println(p1.root)
    //stringToExpressionTree("  ( and  1  ( or(not 1)    1  ) )")
  }
}
