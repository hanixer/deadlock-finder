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

enum UnaryOp:
  case Not

  override def toString: String = this match
    case Not => "!"
