package Models

class Argument {

  /* Private Fields */

  private var _argType:String = _
  private var _argName:String = _


  /* Constructor */

  def this(argType:String, argName:String){
    this()

    _argType = argType
    _argName = argName
  }


  /* Methods */

  def getType():String ={
    return _argType
  }

  def getName():String ={
    return _argName
  }


  def setType(argType:String): Unit ={
    _argType = argType
  }

  def setName(argName:String): Unit ={
    _argName = argName
  }

}
