/*
   --------------------
 /     Drew Darwin      \
| Written in Scala 2.9.1 |
 \     Spring 2012      /
   --------------------
*/

import scala.io.Source

object BoolSimp {

  val DEBUG = false

  val introString = """
   --------------------------------------------------------------------------
 /--------------------------------- BoolSimp ---------------------------------\
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
|                                                                              |
| ** To access the general help menu, type 'help', without the quotes.         |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val helpString = """
   --------------------------------------------------------------------------
 /------------------------------- General Help -------------------------------\
|------------------------------------------------------------------------------|
| General Command Syntax:                                                      |
|   keyword argument1 argument2                                                |
|                                                                              |
| Valid Keywords Include:                                                      |
|   quit                                                                       |
|   help                                                                       |
|   evalxp                                                                     |
|                                                                              |
| Valid Entries for argument1 Include:                                         |
|   Any Keyword                                                                |
|   Some Expression                                                            |
|   ** Note that argument1 is optional when the associated keyword = 'help'    |
|                                                                              |
| Valid Entries for argument2 Include:                                         |
|   Binding List... (e.g. (x-1 z-0 y-1) )                                      |
|   ** Note that argument2 is optional                                         |
|                                                                              |
| Example Commands:                                                            |
|   quit                                    --> Exits BoolSimp                 |
|   help                                    --> Displays this help menu        |
|   help evalxp                             --> Displays the evalxp help menu  |
|   evalxp (and y 1)                        --> Returns 'y'                    |
|   evalxp (or x 1) (x-0)                   --> Returns '1'                    |
|   evalxp (not (and a b)) (a-0 b-1)        --> Returns '1'                    |
|                                                                              |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val helpEvalxp = """
   --------------------------------------------------------------------------
 /------------------------------ Help: 'evalxp' ------------------------------\
|------------------------------------------------------------------------------|
| Command Syntax:                                                              |
|   evalxp boolean-expression optional-binding-list                            |
|                                                                              |
| Valid Boolean Expressions Include:                                           |
|   (and x y)                                                                  |
|   (or a b)                                                                   |
|   (not k)                                                                    |
|   ...where x, y, a, b, k, etc. are arguments                                 |
|   ** Note: All arguments must be either an expression or a single character  |
|            i.e. You can't use words as arguments                             |
|                                                                              |
| Binding List Syntax:                                                         |
|   (a-#, b-#, c-#, ...)                                                       |
|   ...where a, b, c, etc. are variables that appear in an expression          |
|      and # represents a truth value, indicated by 1 or 0.                    |
|                                                                              |
| Example Command:                                                             |
|   evalxp (or x (and y z)) (z-1 y-0)          --> Returns 'x'                 |
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
| 'quit' was the first example given in the 'General Help' section.            |
| Try taking a look there. All you have to do is enter 'help' as a command.    |
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
| 'help' will bring you to the help manual...                                  |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val invalidCommand = """
   --------------------------------------------------------------------------
 /--------------------------------- oops... ----------------------------------\
|------------------------------------------------------------------------------|
| All commands must be enclosed by parentheses.                                |
|                                                                              |
| Type 'help' (without the quotes) for further assistance.                     |
|------------------------------------------------------------------------------|
 \----------------------------------------------------------------------------/
   --------------------------------------------------------------------------
"""

  val commandTooShort = """
   --------------------------------------------------------------------------
 /---------------------------------- umm... ----------------------------------\
|------------------------------------------------------------------------------|
| Could you be a little more clear? The command you've entered is too short.   |
|                                                                              |
| Type 'help' (without the quotes) for further assistance.                     |
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









  def evalxp (expression: String, bindingStringParam: String): String = {
    var bindingString = bindingStringParam
    var bindings: Array[Char] = null
    if (bindingString != "") {
      try {
        bindings = stringToBindingList(bindingString)
      } catch {
        case e: Exception => {
          bindings = null
          bindingString = bindingString + " --> **INVALID**"
        }
      }
    } else {
      bindingString = "NONE"
    }
    println("\n" +
" ---------------------------------------------------------- \n" +
"| evalxp                                                   |\n" +
"|----------------------------------------------------------|")

    if (!isValidExpression(expression)) {
      println("" +
"| Invalid Expression: " + "%-36s |".format(expression) + "\n" +
"| Please check the help menu for proper syntax.            |\n" +
" ---------------------------------------------------------- ")
      return expression
    }

    println("" +
"| Expression: " + "%-44s |".format(expression) + "\n" +
"| Bindings: " + "%-46s |".format(bindingString) + "\n" +
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
      if (isValidBindingList(bindings)) {
        val expressionTree = stringToExpressionTree(expression)
        applyBindings(expressionTree, bindings)
        val newExpression = applyBindings(expression, bindings)
        println("| After binding, expression becomes:                       |\n" +
                "|   %-54s |".format(expressionTreeToString(expressionTree)))
        //Convert expression to ExpressionTree
          //val expressionTree = stringToExpressionTree(expression)
        //Simplify Expression
          val simplifiedExpressionTree = simplifyExpression(expressionTree)
          val simplifiedExpressionString = expressionTreeToString(simplifiedExpressionTree)
          println("| Expression then simplifies to:                           |\n" +
                  "|   %-54s |".format(simplifiedExpressionString))
          println(" ---------------------------------------------------------- ")
      } else {
        println("Invalid binding list")
      }
    }

    return ""
  }

  def applyBindings (expression: String, bindings: Array[Char]): String = {
    //Convert expression string to ExpressionTree
      val expressionTree = stringToExpressionTree(expression)
    return applyBindings(expressionTree, bindings)
  }
  def applyBindings(expressionTree: ExpressionTree, bindings: Array[Char]): String = {
    val DEBUGTHIS = false
    if (DEBUGTHIS) println("--- Beginning call to applyBindings ---")
    //**Note** This is a recursive method
    //Determine the operator of expressionTree
    if (DEBUGTHIS) println("| Determine the operator of the expression")
    if (expressionTree.root == "and" || expressionTree.root == "or") {
      if (DEBUGTHIS) println("|   Operator determined to be either 'and' or 'or'")
      //Check left ExpressionTree
      if (expressionTree.left.root == "and" ||
          expressionTree.left.root == "or" ||
          expressionTree.left.root == "not") {
        if (DEBUGTHIS) println("| Apply Bindings to left subtree")
        applyBindings(expressionTree.left, bindings)
      } else {
        //expressionTree.left.root should be a single character
          if (expressionTree.left.root.length > 1) {
            throw new Error("applyBindings encountered encountered a supposed parameter (" +
                            expressionTree.left.root + ") that was longer than a single " +
                            "character")
          }
        //Search through binding list...
          if (DEBUGTHIS) println("| Search through the binding list...")
          var i = 0
          while (i < bindings.length-1) {
            if (DEBUGTHIS) println("|   Check " + bindings(i) + ", located at index " + i)
            if ((bindings(i) + "") == expressionTree.left.root) {
              if (DEBUGTHIS) println("|   Found a match!")
              expressionTree.left.root = bindings(i+1) + ""
              if (DEBUGTHIS) println("|     Change existing symbol to " + bindings(i+1))
            }
            i += 2
          }
      }
      //Check right ExpressionTree
      if (expressionTree.right.root == "and" ||
          expressionTree.right.root == "or" ||
          expressionTree.right.root == "not") {
        if (DEBUGTHIS) println("| Apply Bindings to right subtree")
        applyBindings(expressionTree.right, bindings)
      } else {
        //expressionTree.right.root should be a single character
          if (expressionTree.right.root.length > 1) {
            throw new Error("applyBindings encountered encountered a supposed parameter (" +
                            expressionTree.right.root + ") that was longer than a single " +
                            "character")
          }
        //Search through binding list...
          if (DEBUGTHIS) println("| Search through the binding list...")
          var i = 0
          while (i < bindings.length-1) {
            if (DEBUGTHIS) println("|   Check " + bindings(i) + ", located at index " + i)
            if ((bindings(i) + "") == expressionTree.right.root) {
              if (DEBUGTHIS) println("|   Found a match!")
              expressionTree.right.root = bindings(i+1) + ""
              if (DEBUGTHIS) println("     Change existing symbol to " + bindings(i+1))
            }
            i += 2
          }
      }

    } else if (expressionTree.root == "not") {
      if (DEBUGTHIS) println("|   Operator determined to be 'not'")
      //Check left ExpressionTree
      if (expressionTree.left.root == "and" ||
          expressionTree.left.root == "or" ||
          expressionTree.left.root == "not") {
        if (DEBUGTHIS) println("| Apply Bindings to left subtree")
        applyBindings(expressionTree.left, bindings)
      } else {
        //expressionTree.left.root should be a single character
          if (expressionTree.left.root.length > 1) {
            throw new Error("applyBindings encountered encountered a supposed parameter (" +
                            expressionTree.left.root + ") that was longer than a single " +
                            "character")
          }
        //Search through binding list...
          if (DEBUGTHIS) println("| Search through the binding list...")
          var i = 0
          while (i < bindings.length-1) {
            if (DEBUGTHIS) println("|   Check " + bindings(i) + ", located at index " + i)
            if ((bindings(i) + "") == expressionTree.left.root) {
              if (DEBUGTHIS) println("|   Found a match!")
              expressionTree.left.root = bindings(i+1) + ""
              if (DEBUGTHIS) println("|     Change existing symbol to " + bindings(i+1))
            }
            i += 2
          }
      }

    } else {
      throw new Error("Received invalid ExpressionTree as input")
    }
    return ""
  }

  def isValidBindingList(bindings: Array[Char]): Boolean = {
    for (i <- 0 until bindings.length) {
      if (i == 0) {
        if (bindings(i) == '0' || bindings(i) == '1')
          return false
      } else {
        if (bindings(i) == '0' || bindings(i) == '1') {
          if (bindings(i-1) == '0' || bindings(i-1) == '1')
            return false
        } else {
          if (bindings(i-1) != '0' && bindings(i-1) != '1')
            return false
        }
      }
    }
    return true
  }



  def stringToBindingList (input: String): Array[Char] = {
    val DEBUGTHIS = DEBUG
    if (DEBUGTHIS) println("------ Beginning call to stringToBindingList -----")
    /*
    Binding list should look like this...
      (x-1 y-0 d-1 j-1)
    */
    var bindings = new Array[Char]((input.length)/2) //could probably be (length-1)/2
    if (DEBUGTHIS) println("| Created bindings: Array[Char] with size: " + input.length/2)
    var previousWasSymbol = false
    var bindingIndex = 0
    if (input.charAt(0) != '(') {
      if (DEBUGTHIS) println("| Throw exception because input did not begin with an " +
                             "open paren")
      throw new Exception("Input did not begin with an open paren")
    }
    if (input.charAt(input.length-1) != ')') {
      if (DEBUGTHIS) println("| Throw exception because input did not end with a " +
                            "closing paren")
      throw new Exception("Input did not end with a closing paren")
    }

    if (DEBUGTHIS) println("| Entering for loop")
    for (i <- 1 until input.length) {
      if (DEBUGTHIS) println("|   i = " + i)
      val previousChar = input.charAt(i-1)
      if (DEBUGTHIS) println("|     previousChar = " + previousChar)
      if (previousChar == '(' || previousChar == ' ') {
        //We have a symbol
          if (DEBUGTHIS) println("|     " + input.charAt(i) + " should be a symbol because "+
                                 "previousChar was " + previousChar)
          if (input.charAt(i) == '0' || input.charAt(i) == '1') {
            throw new Exception("Encountered a " + input.charAt(i) + " where there " +
                                "should have been a symbol")
          }
          if (previousWasSymbol)
            throw new Exception("Encountered two symbols in a row")
        //Add it to the character array
          if (DEBUGTHIS) println("|     Add " + input.charAt(i) + " to bindings at index: " +
                                 bindingIndex)
          bindings(bindingIndex) = input.charAt(i)
          bindingIndex += 1
          previousWasSymbol = true
          if (DEBUGTHIS) println("|     previousWasSymbol now set to " + previousWasSymbol)
      } else if (previousChar == '-') {
        //We have a 0 or 1
          if (input.charAt(i) != '0' && input.charAt(i) != '1')
            throw new Exception("Encountered '" + input.charAt(i) + "' where there " +
                                "should have been a 1 or 0")
          if (!previousWasSymbol)
            throw new Exception("Encountered two 0/1 in a row")
        //Add it to the character array
          if (DEBUGTHIS) println("|     Add " +input.charAt(i) + " to bindings at index: " +
                                 bindingIndex)
          bindings(bindingIndex) = input.charAt(i)
          bindingIndex += 1
          previousWasSymbol = false
          if (DEBUGTHIS) println("|     previousWasSymbol now set to " + previousWasSymbol)
      } else {
        if (!(input.charAt(i) == '(') &&
            !(input.charAt(i) == ')') &&
            !(input.charAt(i) == ' ') &&
            !(input.charAt(i) == '-'))
          throw new Exception("Encountered a symbol or truth value that took up more than " +
                              "one character")
        //Invalid input string
        /*
        throw new Error("stringToBindingList received an invalid input string\n" +
                        "   " + input)
        */
      }
    }
    if (DEBUGTHIS) {
      println("| Resulting binding list is:")
      var debugOutput = "  |"
      for (i <- 0 until bindings.length) {
        debugOutput = debugOutput + bindings(i) + "|"
      }
      println("   " + debugOutput)
      println("------ Ending call to stringToBindingList ------")
    }
    return bindings
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





  def extractBindingString(command: String, expressionStartIndex: Int, expressionLength: Int): String = {
    var bindingListStartIndex = expressionStartIndex + expressionLength + 1
    if (command.length > bindingListStartIndex + 1)
      return command.substring(bindingListStartIndex, command.length)
    else
      return ""
  }




  def extractExpression(command: String): String = {
    //For now, assume command will always be an evalxp command
    var startingIndex = 0
    while (startingIndex < command.length-1 && command.charAt(startingIndex) != ' ') {
      startingIndex += 1
    }

    var parenCount = 0
    var index = startingIndex + 1
    var expression = ""
    do {
      if (command.charAt(index) == '(')
        parenCount += 1
      else if (command.charAt(index) == ')')
        parenCount -= 1
      expression = expression + command.charAt(index)
      index += 1
    } while (parenCount > 0)
    return expression
  }





  def main(args: Array[String]) {
    println(introString)
    var command = readLine("\nEnter a Command -> ")
    while (command != "quit") {
      command = command.trim
      command = normalizeExpressionString(command)
      if (DEBUG) println(command)
      if (command.length < 4) {
        println(commandTooShort)
      } else if (command.substring(0, 4) == "help") {
        if (command.contains("evalxp"))
          println(helpEvalxp)
        else if (command.substring(4, command.length).contains("help"))
          println(helpHelp)
        else if (command.contains("quit"))
          println(helpQuit)
        else
          println(helpString)
      } else if (command.length > 7 && command.substring(0, 7) == "evalxp ") {
        var expression = ""
        val expressionStartIndex = 7
        var bindingString = ""
        var bindingListStartIndex = expressionStartIndex //+ expression.length + 1

        if (command.length >= 9 && command.substring(7, 9) == "p1") {
          //Extract Expression
            expression = expressionTreeToString(p1)
          //Extract Binding List
            bindingString = extractBindingString(command,
                                                 expressionStartIndex,
                                                 2)
          if (DEBUG) println("bindings = " + bindingString)
          evalxp(expression, bindingString)

        } else if (command.length >= 9 && command.substring(7, 9) == "p2") {
          //Extract Expression
            expression = expressionTreeToString(p2)
          //Extract Binding List
            bindingString = extractBindingString(command,
                                                 expressionStartIndex,
                                                 2)
          if (DEBUG) println("bindings = " + bindingString)
          evalxp(expression, bindingString)

        } else if (command.length >= 9 && command.substring(7, 9) == "p3") {
          //Extract Expression
            expression = expressionTreeToString(p3)
          //Extract Binding List
            bindingString = extractBindingString(command,
                                                 expressionStartIndex,
                                                 2)
          if (DEBUG) println("bindings = " + bindingString)
          evalxp(expression, bindingString)

        } else {
            if (command.charAt(7) != '(' || command.length < 9) {
              println(commandEntryError)
            } else {
              //Extract Expression
                try {
                  expression = extractExpression(command)
                } catch {
                  case e: StringIndexOutOfBoundsException => expression = command.substring(7, command.length)
                }
              //Extract Binding List
                bindingString = extractBindingString(command,
                                                     expressionStartIndex,
                                                     expression.length)
              if (DEBUG) println("bindings = " + bindingString)
              evalxp(expression, bindingString)
            }
        }
      } else {
        println(commandEntryError)
      }
      command = readLine("\nEnter a Command -> ")
    }
    if (command == "quit")
      println(thanks)
  }
}
