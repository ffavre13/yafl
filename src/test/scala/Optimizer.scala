
import yafl.SourceFile
import yafl.optimizer.Optimizer
import yafl.parser.Parser
import yafl.syntax.{Syntax, TermTree}
import yafl.typer.{TypedProgram, Typer}

final class OptimizerTests extends munit.FunSuite:

  test("constant folding"):
    val optimized = optimize("1 + 2 + 3")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(6) => ()
  
  test("Boolean constant folding"):
    val optimized = optimize("1 >= 2")
    (optimized.syntax.value : @unchecked) match
      case TermTree.BooleanLiteral(false) => ()

  test("Boolean constant folding2"):
    val optimized = optimize("true && true")
    (optimized.syntax.value : @unchecked) match
      case TermTree.BooleanLiteral(true) => ()

  test("inling"):
    val optimized = optimize("((x : Int) => x) 42")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(42) => ()

  test("inlining then folding"):
    val optimized = optimize("((x : Int) => x + x) 2")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(4) => ()
      
  test("normalization test1"):
    import TermTree.TermApplication as F
    import TermTree.Binding as B
    val optimized = optimize("let x = 0; x + 1")
    (optimized.syntax.value : @unchecked) match
      case B(_, _, Syntax(F(lhs, Syntax(TermTree.Variable("x"), _)), _)) =>
        (lhs.value : @unchecked) match
          case F(_, Syntax(TermTree.IntegerLiteral(1), _)) => ()
        
  test("normalization test2"):
    import TermTree.TermApplication as F
    import TermTree.Binding as B
    val optimized = optimize("let x = 0; 1 + x + 2")
    (optimized.syntax.value : @unchecked) match
      case B(_, _, Syntax(F(Syntax(F(_, Syntax(TermTree.IntegerLiteral(3), _)), _), Syntax(TermTree.Variable("x"), _)), _)) => ()

  test("normalization test3"):
    import TermTree.TermApplication as F
    import TermTree.Binding as B
    val optimized = optimize("1 + (let x = 1 ; x)")
    (optimized.syntax.value : @unchecked) match
      case B(_, _, Syntax(F(lhs, Syntax(TermTree.Variable("x"), _)), _)) =>
        (lhs.value : @unchecked) match
          case F(_, Syntax(TermTree.IntegerLiteral(1), _)) => ()

  test("normalization test4"):
    import TermTree.TermApplication as F
    import TermTree.Binding as B
    val optimized = optimize("(let x = 1 ; x) + 2")
    (optimized.syntax.value : @unchecked) match
      case B(_, _, Syntax(F(_, Syntax(TermTree.Variable("x"), _)), _)) => ()

  test("normalization test5"):
    import TermTree.TermApplication as F
    import TermTree.Binding as B
    val optimized = optimize("let x = 0; 1 + x + 2 + 3")
    (optimized.syntax.value : @unchecked) match
      case B(_, _, Syntax(F(Syntax(F(_, Syntax(TermTree.IntegerLiteral(6), _)), _), Syntax(TermTree.Variable("x"), _)), _)) => ()

  /** Compiles `input` to a WebAssembly module and returns an instance of it. */
  private def optimize(input: String): TypedProgram =
    Optimizer.optimize(Typer.check(Parser.parse(SourceFile("test", input))))

end OptimizerTests