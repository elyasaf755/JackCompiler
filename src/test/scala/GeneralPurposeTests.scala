import Kind.Kind
import org.scalatest.FunSuite

import scala.collection.mutable

class GeneralPurposeTests extends FunSuite{
  test("EnumsValues"){

    assert(Set(Kind.STATIC, Kind.FIELD).subsetOf(Kind.values))

    assert(!Set(Kind.ARG, Kind.VAR).subsetOf(Set(Kind.STATIC, Kind.FIELD)))

    assert(Set(Kind.ARG, Kind.VAR).contains(Kind.ARG))
  }

  test("HashMap"){
    var _hMap = new mutable.HashMap[Kind,Int]()
    println(_hMap)

    _hMap += (Kind.ARG -> 0)
    println(_hMap)

    println(_hMap(Kind.ARG))
    _hMap(Kind.ARG) += 1
    println(_hMap)
  }
}
