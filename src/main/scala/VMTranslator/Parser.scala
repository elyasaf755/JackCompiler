package Tools

import java.io.File

import Tools.CommandType.CommandType
import scala.collection.mutable.ListBuffer
import scala.io.Source

class Parser(){

  //Fields
  private var _inputFileUrl:String = _
  private var _commandsList:ListBuffer[String] = _
  private var _currentCommand:Command = _
  private var _currentFunction:String = _

  //Constructor
  def this(inputFileUrl:String){
    this()

    // if the file ends with ".vm"
    if (inputFileUrl.endsWith(".vm")){
      this._inputFileUrl = inputFileUrl
      this._commandsList = new ListBuffer[String]

      val temp = Source.fromFile(inputFileUrl).getLines().toArray
      temp.foreach(line =>{
        val command = getCoreCommand(line)

        if (command != null.asInstanceOf[String])
          this._commandsList += command
      })

      // if the commands list isn't empty
      if (!this._commandsList.isEmpty)
        this._currentCommand = new Command("", 0)

      _currentFunction = getFileName()
    }
  }


  //Core Methods

  // Returns true if there are more commands to parse
  def hasMoreCommands():Boolean ={
    return _currentCommand.commandNumber <= _commandsList.length
  }

  /*
   * Reads the next command from the file and makes it the current command.
   * Returns true if it advanced successfully.
   */
  def advance():Boolean ={
    val i = _currentCommand.commandNumber + 1

    if (i <= _commandsList.length) {

      _currentCommand = new Command(_commandsList(i - 1), i)

      if (getCommandType() == CommandType.C_FUNCTION)
        _currentFunction = arg1()

      return true
    }

    _currentCommand = new Command("", i)

    return false
  }

  /*
   * Returns the command type of the current command.
   */
  def getCommandType():CommandType ={
    val cmdString = _currentCommand.fullCommandString

    if (stringStartsWithEither(cmdString,"add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not"))
      return CommandType.C_ARITHMETIC

    if (cmdString.startsWith("push"))
      return CommandType.C_PUSH

    if (cmdString.startsWith("pop"))
      return CommandType.C_POP

    if (cmdString.startsWith("label"))
      return CommandType.C_LABEL

    if (cmdString.startsWith("goto"))
      return CommandType.C_GOTO

    if (cmdString.startsWith("if-goto"))
      return CommandType.C_IF

    if (cmdString.startsWith("function"))
      return CommandType.C_FUNCTION

    if (cmdString.startsWith("return"))
      return CommandType.C_RETURN

    if (cmdString.startsWith("call"))
      return CommandType.C_CALL

    return CommandType.C_ERROR
  }

  /*
   * Returns the first argument of the current command.
   * In case of C_ARITHMETIC, the command itself is returned.
   * Should NOT be called if the current command is C_RETURN.
   */
  def arg1():String ={
    var words = _currentCommand.fullCommandString.split("[ ]+")

    if (getCommandType() == CommandType.C_ARITHMETIC)
      return words(0)

    if (getCommandType() == CommandType.C_RETURN)
      return null.asInstanceOf[String]

    //This should be the last condition in this function.
    if (words.length >= 2)
      return words(1)
    else
      return null.asInstanceOf[String]
  }

  /*
   * Returns the second argument of the current command.
   * Should be called only if the current command is
   * C_PUSH / C_POP / C_FUNCTION or C_CALL.
   */
  def arg2():Int ={
    var words = _currentCommand.fullCommandString.split("[ ]+")

    if (getCommandType() == CommandType.C_PUSH ||
      getCommandType() == CommandType.C_POP ||
      getCommandType() == CommandType.C_FUNCTION ||
      getCommandType() == CommandType.C_CALL) {

      return words(2).toInt
    }

    return null.asInstanceOf[Int]
  }

  def getCurrentFunction():String ={
    return _currentFunction
  }

  def getFileName():String ={
    if (_inputFileUrl.endsWith(".vm"))
      return new File(_inputFileUrl).getName.split(".vm")(0)

    return null.asInstanceOf[String]
  }


  // Helper Private Methods

  /*
   * Returns true if string starts with at least of the strings given.
   */
  private def stringStartsWithEither(string:String, strings:String*): Boolean ={
    strings.foreach(x => {
      if (string.endsWith(x))
        return true
    })

    return false
  }

  /*
   * Removes white spaces and comments, and returns a command with no extra white spaces.
   * e.g - "   push   local 2    // bla  bla   " becomes "push local 2".
   * If the whole line is a comment, returns null.
   */
  private def getCoreCommand(line:String):String ={
    var res = line.trim

    if (res.startsWith("//"))
      return null.asInstanceOf[String]

    res = res.split("[ ]*//[ ]*")(0)

    if (res.isEmpty)
      return null.asInstanceOf[String]

    val list = res.split("[ ]+")

    res = ""
    list.foreach(ele =>{
      res += ele + " "
    })

    return res.trim
  }
}

/*
 * A command represents a line of assembly code.
 */
class Command{
  var fullCommandString:String = _
  var commandNumber:Int = _
  var commandType:CommandType = _
  var commandName:String = _
  var arg1:String = _
  var arg2:Int = _

  def this(commandString:String, commandNumber:Int){
    this()

    this.fullCommandString = commandString
    this.commandNumber = commandNumber
  }
}

object CommandType extends Enumeration
{
  type CommandType = Value

  // Assigning values
  val C_ARITHMETIC = Value("C_ARITHMETIC")
  val C_PUSH = Value("push")
  val C_POP = Value("pop")
  val C_LABEL = Value("C_LABEL")
  val C_GOTO = Value("C_GOTO")
  val C_IF = Value("C_IF")
  val C_FUNCTION = Value("C_FUNCTION")
  val C_RETURN = Value("C_RETURN")
  val C_CALL = Value("C_CALL")
  val C_ERROR = Value("C_ERROR")
}

object ArithmeticType extends Enumeration
{
  type ArithmeticType = Value

  // Assigning values
  val ADD = Value("add")
  val SUB = Value("sub")
  val EQUAL = Value("eq")
  val LESS_THAN = Value("lt")
  val GREATER_THATN = Value("gt")
  val NEGATE = Value("neg")
  val AND = Value("and")
  val OR = Value("or")
}

object SegmentType extends Enumeration
{
  type SegmentType = Value

  // Assigning values
  val LCL = Value("LCL")
  val ARG = Value("ARG")
  val THIS = Value("THIS")
  val THAT = Value("THAT")
  val CONSTANT = Value("CONSTANT")
  val STATIC = Value("STATIC")
  val POINTER = Value("POINTER")
  val TEMP = Value("TEMP")
}


