import Kind.Kind

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SymbolTable {

  /* Private Fields */

  private var _classTable = new ListBuffer[Identifier]
  private var _subroutineTable = new ListBuffer[Identifier]

  private val _varCounts:mutable.HashMap[Kind, Int] = mutable.HashMap(
    Kind.STATIC -> 0,
    Kind.FIELD -> 0,
    Kind.ARG -> 0,
    Kind.VAR -> 0,
  )


  /* Methods */

  /*
   * Starts a new subroutine scope
   * (i.e - resets the subroutine's symbol table)
   */
  def startSubroutine(): Unit ={

  }

  /*
   * Defines a new identifier of the given name, type, and kind,
   * and assigns it a running index.
   * STATIC & FIELD  identifiers have a class scope,
   * while ARG & VAR identifiers have a subroutine scope.
   */
  def define(name:String, identifierType:String, kind:Kind):Unit ={

    if (Set(Kind.STATIC, Kind.FIELD).contains(kind)){
      _classTable.addOne(new Identifier(name, identifierType, kind, getVarCount(kind)))
      incKindCount(kind)
    }
    else if (Set(Kind.ARG, Kind.VAR).contains(kind)){
      _subroutineTable.addOne(new Identifier(name, identifierType, kind, getVarCount(kind)))
      incKindCount(kind)
    }

  }

  /*
   * Returns the number of variables of the given kind
   * already defines in the current scope
   */
  def getVarCount(kind:Kind): Int ={
    return _varCounts(kind)
  }

  /*
   * Returns the kind of the named identifier in the current scope.
   * If the identifier if unknown in the current scope, returns NONE.
   */
  def kindOf(name:String):Kind ={

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
    return Kind.NONE
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
  private def incKindCount(kind:Kind): Unit ={
    _varCounts(kind) += 1
  }
}

class Identifier {

  /* Private Fields */

  private var _name:String = _
  private var _identifierType:String = _
  private var _kind:Kind = _
  private var _index:Int = _


  /* Constructor */

  def this(name:String, identifierType:String, kind:Kind, index:Int){
    this()

    _name = name
    _identifierType = identifierType
    _kind = kind
    _index = index
  }


  /* Methods */

  def getName(): String ={
    return _name
  }

  def getType(): String ={
    return _identifierType
  }

  def getKind(): Kind ={
    return _kind
  }

  def getIndex(): Int ={
    return _index
  }
}

object Kind extends Enumeration {
  type Kind = Value

  // Assigning values
  val STATIC = Value("static")
  val FIELD = Value("field")
  val ARG = Value("arg")
  val VAR = Value("var")
  val NONE = Value("none")
}
