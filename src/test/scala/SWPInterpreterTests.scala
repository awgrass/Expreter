import org.scalatest.FunSuite
import scala.util.parsing.combinator._

class SWPInterpreterTests extends FunSuite {

  test("Parser minimal example") {
    val prog = """
    {}
    42
    """
    if(SWPInterpreter.checkProgramGrammer(prog)) {
      assert(true)
    } else {
      fail(SWPInterpreter.checkProgramGrammerStringResult(prog))
    }
  }

  test("Parser short program") {
    val prog = """
    {fun concat(xs, ys)=if eq?(xs,[]) then ys else build(first(xs), concat(rest(xs),ys));}
    concat([1,2,3],[4,5])
    """
    if(SWPInterpreter.checkProgramGrammer(prog)) {
      assert(true)
    } else {
      fail(SWPInterpreter.checkProgramGrammerStringResult(prog))
    }
  }

  test("Parser defect program") {
    val prog = """
    {fun concat(xs, ys)=if eq?(xs,[] then ys else build(first(xs), concat(rest(xs),ys));}
    concat([1,2,3],[4,5])
    """
    assert(! SWPInterpreter.checkProgramGrammer(prog))
  }

  test("Interpreter program with only build in functions") {
    val prog = """
    {}
    if eq?([1],build(1,[])) then add(3, negate(1)) else add(4,2)
    """
    assertResult("2"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }


  test("Interpreter short program") {
    val prog = """
    {fun concat(xs, ys)=if eq?(xs,[]) then ys else build(first(xs), concat(rest(xs),ys));}
    concat([1,2,3],[4,5])
    """
    assertResult("[1,2,3,4,5]"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }

  test("Interpreter let expr") {
    val prog = """
    {}
    let
      val a = 10;
      val b = add(a,22);
    in
      add(a,b)
    end
    """
    assertResult("42"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }

  test("Interpreter refs simple") {
    val prog = """
    {}
    let
      val y = ref(10);
    in
      {
        y := 100;
        !y
      }
    end
    """
    assertResult("100"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }

  test("Interpreter refs") {
    val prog = """
    {}
    let
      val x = ref(add(5,1));
      val y = ref(10);
    in
      {
        x := add(!y, 20);
        y := 100;
        !x
      }
    end
    """
    assertResult("30"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }

}
