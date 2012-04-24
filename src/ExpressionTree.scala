//package hw3

class ExpressionTree(var root: String, val left: ExpressionTree, val right: ExpressionTree) {

  def isEmpty: Boolean = {
    if (root == null) return true
    return false;
  }

  def expressionType = root

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
  override def toString: String = {
    var output = ""
    if (left == null)
      output = output + root
    else
      output = output + "  " + root
    if (left != null)
      if (right != null)
        output = output + "\n " + left + "   " + right + "\n"
      else
        output = output + "\n  " + left
    return output
  }
}
