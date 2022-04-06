package deadlockFinder
package expr

enum BinaryOp:
  case Plus, Minus, Times, Divide, Less, Greater, LessEquals, GreaterEquals, Equals, And, Or

  override def toString: String = this match
    case Plus          => "+"
    case Minus         => "-"
    case Times         => "*"
    case Divide        => "/"
    case Less          => "<"
    case Greater       => ">"
    case LessEquals    => "<="
    case GreaterEquals => ">="
    case Equals        => "=="
    case And           => "&&"
    case Or            => "||"

  def isRelation: Boolean = this match
    case Less => true
    case Greater => true
    case LessEquals => true
    case GreaterEquals => true
    case Equals => true
    case _ => false
    
  def isArithmetic: Boolean = this match
    case Plus => true
    case Minus => true
    case Times => true
    case Divide => true
    case _ => false

enum UnaryOp:
  case Not

  override def toString: String = this match
    case Not => "!"
