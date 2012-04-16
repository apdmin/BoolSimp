//package hw3

class ExpressionTree(val root: String, val left: ExpressionTree, val right: ExpressionTree) {

  /*
  def isEmpty: Boolean = {
    if (root == null) return true
    return false;
  }

  def expressionType: String = root
  */

  /*
  def isValidExpression: Boolean = {
    if (left == null) return false
    if (((root equals "and") || (root equals "or")) && right == null) return false

    root match {
      case "and" => return left.isValidExpression && right.isValidExpression
      case "or" => return left.isValidExpression && right.isValidExpression
      case "not" => return left.isValidExpression && right == null
      case _ => return false
    }
  }
  */
}
