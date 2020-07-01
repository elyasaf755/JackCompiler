package Models

import JackAnalyzer.VarKind.VarKind

class Identifier {

  /* Private Fields */

  private var _name:String = _
  private var _identifierType:String = _
  private var _kind:VarKind = _
  private var _index:Int = _


  /* Constructor */

  def this(name:String, identifierType:String, kind:VarKind, index:Int){
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

  def getKind(): VarKind ={
    return _kind
  }

  def getIndex(): Int ={
    return _index
  }


  def setName(name:String): Unit ={
    _name = name
  }

  def setType(identifierType:String): Unit ={
    _identifierType = identifierType
  }

  def setKind(kind:VarKind): Unit ={
    _kind = kind
  }

  def setIndex(index:Int): Unit ={
    _index = index
  }
}
