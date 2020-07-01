import JackAnalyzer.VarKind
import JackAnalyzer.VarKind.VarKind
import Modules.JackAnalyzerClasses.LabelKind
import org.scalatest.FunSuite

import scala.collection.mutable

class GeneralPurposeTests extends FunSuite{
  test("EnumsValues"){

    assert(Set(VarKind.STATIC, VarKind.FIELD).subsetOf(VarKind.values))

    assert(!Set(VarKind.ARG, VarKind.VAR).subsetOf(Set(VarKind.STATIC, VarKind.FIELD)))

    assert(Set(VarKind.ARG, VarKind.VAR).contains(VarKind.ARG))
  }

  test("HashMap"){
    var _hMap = new mutable.HashMap[VarKind,Int]()
    println(_hMap)

    _hMap += (VarKind.ARG -> 0)
    println(_hMap)

    println(_hMap(VarKind.ARG))
    _hMap(VarKind.ARG) += 1
    println(_hMap)
  }

  test("Chars"){
    assert('H'.toInt == 72)
  }

  test("functions"){
    var x = 5
    var b = foo()
    assert(b == 5)
  }

  private def foo(): Int ={
    return 5
  }
}
