//package hw3
//import hw3.ExpressionTree
import scala.io.Source

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
  val p1FirstParam = new ExpressionTree("x", null, null)
  val p1SecondParam = new ExpressionTree(
      "or",
      (new ExpressionTree("x", null, null)),
      (new ExpressionTree
        (
          "and",
          (new ExpressionTree("y", null, null)),
          (new ExpressionTree
            (
              "not",
              (new ExpressionTree("z", null, null)),
              null
            )
          )
        )
      )
    )
  val p1 = new ExpressionTree("and", p1FirstParam, p1SecondParam)
  val p2 = stringToExpressionTree("(and (and z 0) (or x 1))")
  val p3 = stringToExpressionTree("(or 1 a)")

//--------------------------------------------------------------------------




  def stringToExpressionTree(expressionString: String): ExpressionTree = {
    val DEBUG_THIS = false
    if (DEBUG_THIS) {
      println(" ---------------------------------------------------------- ")
      println("| Beginning call to stringToExpressionTree                 |")
      println("|----------------------------------------------------------|")
      println("| Expression String: " + "%-37s |".format(expressionString))
    }
    //Trim and normalized the expression string
      var normalizedES = expressionString.trim
      normalizedES = normalizeExpressionString(normalizedES)

    if (!isValidExpression(normalizedES))
      throw new Error("stringToExpressionTree received an invalid expression string")

    if (DEBUG_THIS) {
      println("| Normalized Version: " + "%-36s |".format(normalizedES))
      if (normalizedES(0) != '(' || normalizedES(normalizedES.length-1) != ')') {
        println("|..........................................................|")
        println("| That is not a valid expression. All expressions must     |")
        println("| be enclosed by parenthesis.                              |")
        println(" ---------------------------------------------------------- ")
      }
    }

    /*
    Algorithm:
      • Parse through the string.
      • If you see an open paren, need to instantiate a new ExpressionTree
    */

    //Since the first character is an open paren, we can create an ExpressionTree right away
    //Figure out which expression tree to create
    var expressionTree: ExpressionTree = null


    if (normalizedES.substring(1, 4) == "and" || normalizedES.substring(1, 3) == "or") {
      //Extract the first and second parameters
        val firstParam = extractFirstParam(normalizedES)
        val secondParam = extractSecondParam(normalizedES)

      //Convert the first and second parameters to ExpressionTree objects
        var firstParamTree: ExpressionTree = null
        var secondParamTree: ExpressionTree = null
        if (firstParam.charAt(0) != '(')
          firstParamTree = new ExpressionTree(firstParam, null, null)
        else
          firstParamTree = stringToExpressionTree(firstParam)
        if (secondParam.charAt(0) != '(')
          secondParamTree = new ExpressionTree(secondParam, null, null)
        else
          secondParamTree = stringToExpressionTree(secondParam)
  
      //Create the overall ExpressionTree for the method call
        if (normalizedES.substring(1, 4) == "and")
          expressionTree = createAndExpression(firstParamTree, secondParamTree)
        else
          expressionTree = createOrExpression(firstParamTree, secondParamTree)
        return expressionTree



    } else if (normalizedES.substring(1, 4) == "not") {
      //Extract the parameter
        val firstParam = extractFirstParam(normalizedES)

      //Convert the parameter to an ExpressionTree object
        var firstParamTree: ExpressionTree = null
        if (firstParam.charAt(0) != '(')
          firstParamTree = new ExpressionTree(firstParam, null, null)
        else
          firstParamTree = stringToExpressionTree(firstParam)

      //Create the overall ExpressionTree for the method call
        expressionTree = createNotExpression(firstParamTree)
        return expressionTree


    } else {
      throw new Error("stringToExpressionTree reached end of code without returning properly")
    }
  }








  def expressionTreeToString(expressionTree: ExpressionTree): String = {
    var output = ""
    if (expressionTree.root == "and" || expressionTree.root == "or") {
      //Generate First Parameter
      var firstParam = ""
      if (expressionTree.left == null)
        throw new Error ("expressionTree.left == null when it shouldn't be.")
      if (expressionTree.left.left == null)
        firstParam = expressionTree.left.root
      else
        firstParam = expressionTreeToString(expressionTree.left)

      //Generate Second Parameter
      var secondParam = ""
      if (expressionTree.right == null)
        throw new Error ("expressionTree.right == null when it shouldn't be.")
      if (expressionTree.right.left == null)
        secondParam = expressionTree.right.root
      else
        secondParam = expressionTreeToString(expressionTree.right)

      output = output + "(" + expressionTree.root + " " + firstParam + " " + secondParam + ")"
    } else if (expressionTree.root == "not") {
      //Generate Parameter
      var param = ""
      if (expressionTree.left == null)
        throw new Error ("expressionTree.left == null when it shouldn't be.")
      if (expressionTree.left.left == null)
        param = expressionTree.left.root
      else
        param = expressionTreeToString(expressionTree.left)

      output = output + "(" + expressionTree.root + " " + param + ")"
    } else {
      output = expressionTree.root
    }
    return output
  }











  def extractFirstParam(expression: String): String = {
    //We can assume the given expression has already been normalized
    if (expression.substring(1, 4) == "and"
        || expression.substring(1, 3) == "or"
        || expression.substring(1, 4) == "not") {
      var param = ""
      var parenCount = 0
      var index = 0
      if (expression.substring(1, 4) == "and" || expression.substring(1, 4) == "not")
        index = 5
      else
        index = 4
      do {
        if (expression.charAt(index) == '(')
          parenCount += 1
        else if (expression.charAt(index) == ')')
          parenCount -= 1
        param = param + expression.charAt(index)
        index += 1
      } while (parenCount > 0)
      if (expression.charAt(index) != ' ' && expression.charAt(index) != ')')
        throw new Exception("Parameter was not a single character or an expression.")
      return param
    }
    throw new Error ("extractFirstParam encountered an error that prevented it from working")
  }



  def extractSecondParam(expression: String): String = {
    //We can assume the given expression has already been normalized
    //Need to pass over the first parameter
    if (expression.substring(1, 4) == "and" || expression.substring(1, 3) == "or") {
      var param = ""
      var parenCount = 0
      var index = 0
      if (expression.substring(1, 4) == "and")
        index = 5
      else
        index = 4
      if (expression.charAt(index) == '(') {
        //The first parameter must be, in this case, an expression itself.
        do {
          if (expression.charAt(index) == '(')
            parenCount += 1
          else if (expression.charAt(index) == ')')
            parenCount -= 1
          index += 1
        } while (parenCount > 0)
        //Once we get here, make sure parenCount is zero
        parenCount = 0
        index += 1
      } else {
        //The first parameter is a single character and therefore the second parameter
        //starts two indices later
        index += 2
      }
      do {
        if (expression.charAt(index) == '(')
          parenCount += 1
        else if (expression.charAt(index) == ')')
          parenCount -= 1
        param = param + expression.charAt(index)
        index += 1
      } while (parenCount > 0)
      if (expression.charAt(index) != ')')
        throw new Exception("Parameter was neither a single charactor, nor an expression.")
      return param
    }
    throw new Error ("No second parameter should exist in the given expression.")
  }








  def createAndExpression(left: String, right: String): ExpressionTree = {
    val l = new ExpressionTree(left, null, null)
    val r = new ExpressionTree(right, null, null)
    return createAndExpression(l, r)
  }
  def createAndExpression(left: ExpressionTree, right: ExpressionTree): ExpressionTree = {
    return new ExpressionTree("and", left, right)
  }

  def createOrExpression(left: String, right: String): ExpressionTree = {
    val l = new ExpressionTree(left, null, null)
    val r = new ExpressionTree(right, null, null)
    return createOrExpression(l, r)
  }
  def createOrExpression(left: ExpressionTree, right: ExpressionTree): ExpressionTree = {
    return new ExpressionTree("or", left, right)
  }

  def createNotExpression(argument: String): ExpressionTree = {
    val arg = new ExpressionTree(argument, null, null)
    return createNotExpression(arg)
  }
  def createNotExpression(argument: ExpressionTree): ExpressionTree = {
    return new ExpressionTree("not", argument, null)
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








  def simplifyExpression(expression: ExpressionTree): ExpressionTree = {
    //This method recursively simplifies a boolean expression
    if (expression.root == "or") {
      return simplifyOrExpression(expression)
    } else if (expression.root == "and") {
      return simplifyAndExpression(expression)
    } else if (expression.root == "not") {
      return simplifyNotExpression(expression)
    } else {
      return expression
    }
  }








  def simplifyOrExpression(orExpression: ExpressionTree): ExpressionTree = {
    //This method simplifies OR expressions using identies such as (or 1 x) == 1
    //Can safely assume that the given or expression is indeed an or expression
    val firstParameter = simplifyExpression(orExpression.left)
    val secondParameter = simplifyExpression(orExpression.right)
    val firstParamString = expressionTreeToString(firstParameter)
    val secondParamString = expressionTreeToString(secondParameter)

    if (firstParamString == "0")
      return secondParameter
    else if (secondParamString == "0")
      return firstParameter
    else if (firstParamString == "1" || secondParamString == "1")
      return new ExpressionTree("1", null, null)
    else if (firstParamString == secondParamString)
      return firstParameter
    else
      return new ExpressionTree("or", firstParameter, secondParameter)
  }







  def simplifyAndExpression(andExpression: ExpressionTree): ExpressionTree = {
    val firstParameter = simplifyExpression(andExpression.left)
    val secondParameter = simplifyExpression(andExpression.right)
    val firstParamString = expressionTreeToString(firstParameter)
    val secondParamString = expressionTreeToString(secondParameter)

    if (firstParamString == "0" || secondParamString == "0")
      return new ExpressionTree("0", null, null)
    else if (firstParamString == "1")
      return secondParameter
    else if (secondParamString == "1")
      return firstParameter
    else if (firstParamString == secondParamString)
      return firstParameter
    else
      return new ExpressionTree("and", firstParameter, secondParameter)
  }








  def simplifyNotExpression(notExpression: ExpressionTree): ExpressionTree = {
    val parameter = simplifyExpression(notExpression.left)
    val paramString = expressionTreeToString(parameter)

    if (paramString == "1")
      return new ExpressionTree("0", null, null)
    else if (paramString == "0")
      return new ExpressionTree("1", null, null)
    if (paramString.charAt(0) != '(')
      return new ExpressionTree("not", parameter, null)

    //If we've gotten here, we know we are taking the compliment of another expression
    if (paramString.substring(1, 4) == "and") {
      return new ExpressionTree("or",
                                new ExpressionTree("not", parameter.left, null),
                                new ExpressionTree("not", parameter.right, null)
                               )
    } else if (paramString.substring(1, 3) == "or") {
      return new ExpressionTree("and",
                                new ExpressionTree("not", parameter.left, null),
                                new ExpressionTree("not", parameter.right, null)
                               )
    } else {
      return new ExpressionTree("not", parameter, null)
    }
  }









  def evalxp (expression: String, bindings: Array[String]): String = {
    println("\n" +
" ---------------------------------------------------------- \n" +
"| evalxp                                                   |\n" +
"|----------------------------------------------------------|")

    if (!isValidExpression(expression)) {
      println("\n" +
"| Invalid Expression: " + "%36s |".format(expression) + "\n" +
"| Please check the help menu for proper syntax.            |\n" +
" ---------------------------------------------------------- ")
      return expression
    }

    println("" +
"| Expression: " + "%-44s |".format(expression) + "\n" +
"| Bindings: " + "%-46s |".format(bindings) + "\n" +
"|----------------------------------------------------------|")

    if (bindings == null) {
      //Convert expression to expressionTree
        val expressionTree = stringToExpressionTree(expression)
      //Simplify Expression
        val simplifiedExpressionTree = simplifyExpression(expressionTree)
        val simplifiedExpressionString = expressionTreeToString(simplifiedExpressionTree)
        println("| Expression Simplifies To:                                |\n" +
                "|   %-54s |".format(simplifiedExpressionString))
        println(" ---------------------------------------------------------- ")

    } else {
      //Apply Bindings
    }

    return ""
  }

def applyBindings (expression: String, bindings: Array[String]) ={
}







  def isValidExpression(expression: String): Boolean = {
    //Assume the expression has already been normalized
    if (expression == "") {
      if (DEBUG) println("Invalid expression because expression == \"\"")
      return false
    }
    if (expression.charAt(0) != '(') {
      if (DEBUG) println("Invalid because the first character is '" +
                         expression.charAt(0) + "' instead of an open paren")
      return false
    }
    if (expression.charAt(expression.length-1) != ')') {
      if (DEBUG) println("Invalid because expression doesn't end with an open paren")
      return false
    }
    if (expression.length < 7) {
      if (DEBUG) println("Invalid because expression is less than 7 char long")
      return false
    }
    if (expression.substring(1, 4) != "and" &&
        expression.substring(1, 4) != "not" &&
        expression.substring(1, 3) != "or") {
      if (DEBUG) println("Invalid because the first 2-3 chars after open paren are neither " +
                         "'and', 'not', nor 'or'")
      return false
    }
    if (expression.substring(1, 3) == "or" && expression.charAt(3) != ' ') {
      if (DEBUG) println("Invalid because 'or' is not followed by a space")
      return false
    }
    if (expression.substring(1, 4) == "and" || expression.substring(1, 4) == "not") {
      //  ( a n d   ( n o t   1 )  0 )
      //  0 1 2 3 4 5 6 7 8 9

      //  ( n o t   x )
      //  0 1 2 3 4 5 6
      if (expression.charAt(5) == ')' || expression.charAt(5) == '\\') {
        if (DEBUG) println("Invalid because operator was followed by an illegal char")
        return false
      }
      if (expression.substring(1, 4) == "and" && expression.charAt(6) == ')') {
        if (DEBUG) println("Invalid because encountered a closing paren too early in the " +
                           "first parameter")
        return false
      }
      //Extract Parameters
      if (expression.substring(1, 4) == "and") {
        var first = ""
        var second = ""
        try {
          first = extractFirstParam(expression)
          second = extractSecondParam(expression)
        } catch {
          case e: Exception => {
            if (DEBUG) println("Invalid because at least one of the two parameters consists " +
                               "of more than one character")
            return false
          }
        }
        var firstValid = false
        var secondValid = false
        if (first.charAt(0) == '(') {
          firstValid = isValidExpression(first)
        } else {
          if (first.length > 1) {
            if (DEBUG) println("Invalid because the first parameter (" +
                               first + ") is longer than 1 char and not an expression itself")
            firstValid = false
          } else {
            firstValid = true
          }
        }
        if (second.charAt(0) == '(') {
          secondValid = isValidExpression(second)
        } else {
          if (second.length > 1) {
            if (DEBUG) println("Invalid because the second parameter (" +
                               second + ") is longer than 1 char and not an expression itself")
            secondValid = false
          } else {
            secondValid = true
          }
        }
        if (firstValid && secondValid)
          return true
        else
          return false
      } else {
        var first = ""
        try {
          first = extractFirstParam(expression)
        } catch {
          case e: Exception => {
            if (DEBUG) println("Invalid because the parameter consists of more than one " +
                               "character")
              return false
          }
        }
        if (first.charAt(0) == '(') {
          return isValidExpression(first)
        } else {
          if (first.length > 1) {
            if (DEBUG) println("Invalid because the first and only parameter (" +
                               first + ") is longer than 1 char and not an expression itself")
            return false
          } else {
            return true
          }
        }
      }
    } else if (expression.substring(1, 3) == "or") {
      //  ( o r   ( n o t   1 )  0 )
      //  0 1 2 3 4 5 6 7 8 9

      if (expression.charAt(4) == ')' || expression.charAt(4) == '\\') {
        if (DEBUG) println("Invalid because 'or' operator was followed by an illegal char")
        return false
      }
      if (expression.charAt(5) == ')') {
        if (DEBUG) println("Invalid because encountered a closing paren too early in the " +
                           "first parameter")
        return false
      }
      //Extract Parameters
      var first = ""
      var second = ""
      try {
        first = extractFirstParam(expression)
        second = extractSecondParam(expression)
      } catch {
        case e: Exception => {
          if (DEBUG) println("Invalid because at least one of the two parameters consists " +
                            "of more than one character")
          return false
        }
      }
      var firstValid = false
      var secondValid = false
      if (first.charAt(0) == '(') {
        firstValid = isValidExpression(first)
      } else {
        if (first.length > 1) {
          if (DEBUG) println("Invalid because the first parameter (" +
                             first + ") is longer than 1 char and not an expression itself")
          firstValid = false
        } else {
          firstValid = true
        }
      }
      if (second.charAt(0) == '(') {
        secondValid = isValidExpression(second)
      } else {
        if (second.length > 1) {
          if (DEBUG) println("Invalid because the second parameter (" +
                             second + ") is longer than 1 char and not an expression itself")
          secondValid = false
        } else {
          secondValid = true
        }
      }
      if (firstValid && secondValid)
        return true
      else
        return false
    }
    return true
  }








  def main(args: Array[String]) {
    //val andExpression = createAndExpression("1", "0")
    /*
    val expression = "(and x 1)"
    println("First Parameter = '" + extractFirstParam(expression) + "'")
    println("Second Parameter = '" + extractSecondParam(expression) + "'")
    println(stringToExpressionTree(expression))
    println(p1)
    println("p1 = " + expressionTreeToString(p1))
    println("p2 = " + expressionTreeToString(p2))
    println("p3 = " + expressionTreeToString(p3))
    println("-------Time to test----------")
    println("We start with this expression: " + expression)
    //val expressionTree = stringToExpressionTree(expression)
    println("After converting the expression to an ExpressionTree and then")
    println("converting it back, we get: " +
            expressionTreeToString(expressionTree)
           )
    //val simplifiedExpressionTree = simplifyExpression(expressionTree)
    println(expressionTree)
    println(simplifiedExpressionTree)
    println(expressionTreeToString(simplifiedExpressionTree))
    */
    println(introString)
    var command = readLine("\nEnter a Command -> ")
    while (command != "quit") {
      command = command.trim
      command = normalizeExpressionString(command)
      if (command.contains(" help") || command.contains("help "))
        println(helpString)
      if (isValidExpression(command)) {
        //println("Valid Expression!")
        //Convert command to expressionTree
        val expressionTree = stringToExpressionTree(command)
        val simplifiedExpressionTree = simplifyExpression(expressionTree)
        println(expressionTreeToString(simplifiedExpressionTree))
        evalxp(command, null)
      } else {
        println("Invalid Expression")
      }
      command = readLine("\nEnter a Command -> ")
    }
  }
}
