package yafl.optimizer

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}
import yafl.syntax.TermTree.BooleanLiteral

object Optimizer:

  /** Returns `program` optimized.
   *  Loops until no more changes (optimisations) can be done
   */
  def optimize(program: TypedProgram): TypedProgram =
    val (optimized, updated) = constantFoldRecursively(program.syntax, program.types)
    val result = TypedProgram(optimized, updated)
    if optimized == program.syntax then result else optimize(result)
      // TypedProgram(optimized, updated)

    TypedProgram(normalization(optimized), updated)

  /** Substitutes constant expressions in `tree` with their results, returning a an updated syntax
    * tree along with a map from each term to its type.
    */
  private def constantFoldRecursively(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments): (Syntax[TermTree], TypedProgram.TypeAssignments) = {
    constantFold(tree) match
      case Some(s) =>
        // Constant folding succeeded; return the updated tree.
        (s, Map(s -> types(tree)))

      case _ => tree.value match
        case e: TermTree.TermApplication =>
          // Apply the optimization recursively.
          val (f, ts) = constantFoldRecursively(e.abstraction, types)
          val (a, us) = constantFoldRecursively(e.argument, types)
          val updated = Syntax(TermTree.TermApplication(f, a), tree.span)

          // INline first, the fold result if possible
          reduceApp(updated, types ++ ts ++ us) match
            case Some(s) => s
            case _ => constantFold(updated) match
              case Some(s) => (s, Map(s -> types(tree)))
              case _ => (updated, ts ++ us + (updated -> types(tree)))

        case _ =>
          (tree, Map(tree -> types(tree)))
  }

  /* Substitutes all occurrences of a variable with a given syntax tree :
    * e.g.: x =2 -> all x replaced by 2 */
  private def substitute(tree : Syntax[TermTree], name : String, substituteTree : Syntax[TermTree]): Syntax[TermTree] = {
    tree.value match
      case TermTree.Variable(n) if n == name => substituteTree
      case TermTree.TermApplication(f, a) =>
        Syntax(TermTree.TermApplication(
          substitute(f, name, substituteTree),
          substitute(a, name, substituteTree)
        ), tree.span)

      case TermTree.Variable(_) => tree

      case TermTree.TermAbstraction(parameter, ascription, body) =>
        if parameter.value.name == name then tree
        else Syntax(TermTree.TermAbstraction(
          parameter, ascription, substitute(body, name, substituteTree)
        ), tree.span)

      case TermTree.Binding(bind_name, value, body) =>
        val newValue = substitute(value, name, substituteTree)
        if bind_name.value.name == name then Syntax(TermTree.Binding(bind_name, newValue, body), tree.span)
        else Syntax(TermTree.Binding(
            bind_name, newValue, substitute(body, name, substituteTree)
            ),tree.span)

      case TermTree.RecursiveAbstraction(rec_abstraction_name, ascription, body) =>
        if rec_abstraction_name.value.name == name then tree
        else Syntax(TermTree.RecursiveAbstraction(
          rec_abstraction_name, ascription, substitute(body, name, substituteTree)
        ), tree.span)

      case TermTree.Conditional(condition, succes_thenBrench, fail_elseBranch) =>
        Syntax(TermTree.Conditional(
          substitute(condition, name, substituteTree),
          substitute(succes_thenBrench, name, substituteTree),
          substitute(fail_elseBranch, name, substituteTree)
        ), tree.span)

      case TermTree.TypeAbstraction(parameter, body) =>
        Syntax(TermTree.TypeAbstraction(
          parameter, substitute(body, name, substituteTree)
        ), tree.span)

      case TermTree.TypeApplication(abstraction, argument) =>
        Syntax(TermTree.TypeApplication(
          substitute(abstraction, name, substituteTree), argument
        ), tree.span)

      case _ => tree

  }


  /* Check if tree's TermApplication and it's callee is TermAbstraction
      yes = detected pattern of lambda --> calls substitution and return the reduced tree
      no = nothing detected: return None
   */
  private def reduceApp(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] ={
    tree.value match
      case TermTree.TermApplication(Syntax(TermTree.TermAbstraction(param, _, body), _), argument) =>
        val reduced = substitute(body, param.value.name, argument)
        Some((reduced, types + (reduced -> types(tree))))
      case _ =>
        None
    }

  /** Returns a literal denoting the result of `tree` iff it represents a constant expression. */
  private def constantFold(tree: Syntax[TermTree]): Option[Syntax[TermTree]] =
    import TermTree.TermApplication as F
    tree.value match
      case F(Syntax(F(InfixOperator(f), IntegerConstant(lhs)), _), IntegerConstant(rhs)) =>
        f match
          case InfixOperator.Add | InfixOperator.Div | InfixOperator.Mul | InfixOperator.Div =>
            
            val n = f match
              case InfixOperator.Add => lhs + rhs
              case InfixOperator.Sub => lhs - rhs

              case InfixOperator.Mul => lhs * rhs
              case InfixOperator.Div => lhs / rhs
              case _ => 0

            Some(Syntax(TermTree.IntegerLiteral(n), tree.span))
          case _ =>
                      
            val n = f match
              case InfixOperator.Eq => lhs == rhs
              case InfixOperator.Lte => lhs <= rhs
              case InfixOperator.Gte => lhs >= rhs
              case InfixOperator.Neq => lhs != rhs
              case InfixOperator.Lt => lhs < rhs
              case InfixOperator.Gt => lhs > rhs
              case _ => false
              
            Some(Syntax(TermTree.BooleanLiteral(n), tree.span))
      
      case F(Syntax(F(InfixOperator(f), Syntax(BooleanLiteral(lhs),_)), _), Syntax(BooleanLiteral(rhs),_)) =>
        f match
          case InfixOperator.And | InfixOperator.Or =>
            val n = f match
              case InfixOperator.And => lhs && rhs
              case InfixOperator.Or => lhs || rhs
              case _ => false

            Some(Syntax(TermTree.BooleanLiteral(n), tree.span))

          case _ => None
      case _ => None
  
  
  private def normalization(tree: Syntax[TermTree]): Syntax[TermTree] =
    import TermTree.TermApplication as F
    import TermTree.Variable
    import TermTree.Binding as B

    val result = tree.value match
      case b: B =>
        Syntax(B(b.name, normalization(b.initializer), normalization(b.body)), tree.span)

      // Remonte le binding coté rhs
      case F(f, Syntax(b: B, _)) =>
        val newApp = Syntax(F(f, b.body), tree.span)
        Syntax(B(b.name, b.initializer, normalization(newApp)), tree.span)

      // Remonte le binding coté lhs
      case F(Syntax(b: B, _), a) =>
        val newApp = Syntax(F(b.body, a), tree.span)
        Syntax(B(b.name, b.initializer, normalization(newApp)), tree.span)

      // c + (c + x) => (c + c) + x (assosiativity) right
      case F(Syntax(F(Syntax(Variable("infix+"), opSpan), Syntax(F(Syntax(F(Syntax(Variable("infix+"), _), IntegerConstant(c1)), innerSpan), x), _)), outerSpan), IntegerConstant(c2))
          if IntegerConstant.unapply(x).isEmpty =>
        val folded  = Syntax(TermTree.IntegerLiteral(c1 + c2), tree.span)
        val newLeft = Syntax(F(Syntax(Variable("infix+"), opSpan), folded), outerSpan)
        Syntax(F(newLeft, x), tree.span)

      // c + (x + c) => (c + c) + x (assosiativity) left
      case F(Syntax(F(Syntax(Variable("infix+"), op1Span), IntegerConstant(c1)), outerSpan), Syntax(F(Syntax(F(Syntax(Variable("infix+"), _), IntegerConstant(c2)), innerSpan), x), _))
          if IntegerConstant.unapply(x).isEmpty =>
        val folded  = Syntax(TermTree.IntegerLiteral(c1 + c2), tree.span)
        val newLeft = Syntax(F(Syntax(Variable("infix+"), op1Span), folded), outerSpan)
        Syntax(F(newLeft, x), tree.span)

      // x + c => c + x (commutativity)
      case F(Syntax(F(Syntax(Variable("infix+"), opSpan), lhs), innerSpan), rhs)
          if IntegerConstant.unapply(lhs).isEmpty && IntegerConstant.unapply(rhs).nonEmpty =>
        Syntax(F(Syntax(F(Syntax(Variable("infix+"), opSpan), rhs), innerSpan), lhs), tree.span)

      case F(f, a) =>
        Syntax(F(normalization(f), normalization(a)), tree.span)

      case _ =>
        tree

    if result != tree then normalization(result) else result

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant
