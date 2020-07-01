package JackAnalyzer

import JackAnalyzer.VarKind.VarKind
import Models.{Identifier}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SymbolTable {

  /* Private Fields */

  private var _classTable = new ListBuffer[Identifier]()
  private var _subroutineTable = new ListBuffer[Identifier]()

  private val _varCounts:mutable.HashMap[VarKind, Int] = mutable.HashMap(
    VarKind.STATIC -> 0,
    VarKind.FIELD -> 0,
    VarKind.ARG -> 0,
    VarKind.VAR -> 0,
  )


  /* Methods */

  /*
   * Starts a new subroutine scope
   * (i.e - resets the subroutine's symbol table)
   */
  def startSubroutine(): Unit ={
    _subroutineTable.clear()

    val vars = List(VarKind.ARG, VarKind.VAR)

    vars.foreach(v =>{
      _varCounts(v) = 0
    })
  }

  /*
   * Defines a new identifier of the given name, type, and kind,
   * and assigns it a running index.
   * STATIC & FIELD  identifiers have a class scope,
   * while ARG & VAR identifiers have a subroutine scope.
   */
  def define(name:String, identifierType:String, kind:VarKind):Unit ={

    if (Set(VarKind.STATIC, VarKind.FIELD).contains(kind)){
      _classTable.addOne(new Identifier(name, identifierType, kind, getVarCount(kind)))
      incKindCount(kind)
    }
    else if (Set(VarKind.ARG, VarKind.VAR).contains(kind)){
      _subroutineTable.addOne(new Identifier(name, identifierType, kind, getVarCount(kind)))
      incKindCount(kind)
    }

  }

  /*
   * Returns the number of variables of the given kind
   * already defines in the current scope
   */
  def getVarCount(kind:VarKind): Int ={
    return _varCounts(kind)
  }

  /*
   * Returns the kind of the named identifier in the current scope.
   * If the identifier if unknown in the current scope, returns NONE.
   */
  def kindOf(name:String):VarKind ={

    // first look up the subroutine table
    _subroutineTable.foreach(identifier => {
      if (identifier.getName() == name){
        return identifier.getKind()
      }
    })

    // then look up the class table
    _classTable.foreach(identifier => {
      if (identifier.getName() == name){
        return identifier.getKind()
      }
    })

    // return NONE if name wasn't found in both tables
    return VarKind.NONE
  }

  /*
   * Returns the type of the named identifier in the current scope
   */
  def typeOf(name:String):String ={

    // first look up the subroutine table
    _subroutineTable.foreach(identifier => {
      if (identifier.getName() == name){
        return identifier.getType()
      }
    })

    // then look up the class table
    _classTable.foreach(identifier => {
      if (identifier.getName() == name){
        return identifier.getType()
      }
    })

    // return NULL if name wasn't found in both tables
    return null.asInstanceOf[String]
  }

  /*
   * Returns the index assigned to the named identifier
   */
  def indexOf(name:String):Int ={

    // first look up the subroutine table
    _subroutineTable.foreach(identifier => {
      if (identifier.getName() == name){
        return identifier.getIndex()
      }
    })

    // then look up the class table
    _classTable.foreach(identifier => {
      if (identifier.getName() == name){
        return identifier.getIndex()
      }
    })

    // return NULL if name wasn't found in both tables
    return -1
  }

  /* Private Helper Methods */

  /*
   * Increment the current number of given kind in the symbol table by 1
   */
  private def incKindCount(kind:VarKind): Unit ={
    _varCounts(kind) += 1
  }
}

object VarKind extends Enumeration {
  type VarKind = Value

  // Assigning values
  val STATIC = Value("static")
  val FIELD = Value("this")
  val ARG = Value("argument")
  val VAR = Value("local")
  val NONE = Value("none")
}