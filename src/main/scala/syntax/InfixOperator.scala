package yafl.syntax

/** An operator applied with infix notation (e.g., `+` in `1 + 2`). */
enum InfixOperator:

  case Add, Sub
  case Mul, Div, Eq, Lte, Gte, Neq, Lt, Gt

object InfixOperator:

  def unapply(s: Syntax[TermTree]): Option[InfixOperator] =
    s match
      case Syntax(TermTree.Variable(n), _) => n match
        case "infix+" => Some(Add)
        case "infix-" => Some(Sub)

        case "infix*" => Some(Mul)
        case "infix/" => Some(Div)  // Div signed
        case "infix==" => Some(Eq)
        case "infix<=" => Some(Lte) //Less than equal
        case "infix>=" => Some(Gte) //Greater than equal
        case "infix!=" => Some(Neq) //Not equal
        case "infix<" => Some(Lt)   //Less than
        case "infix>" => Some(Gt)   //Greater than
        case _ => None
      case _ => None

end InfixOperator
