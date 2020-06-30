package Tools

import java.io.{FileWriter}

import Tools.CommandType.CommandType
import Tools.SegmentType.SegmentType

class CodeWriter {

  //Private Fields

  private var _outputFileUrl:String = _
  private var _writer:FileWriter = _
  private var _inputFileName:String = _
  private var _currentFunction:String = _

  // counters
  private var _jmp = 1
  private var _call = 1
  private var _ret = 1


  //Constructors

  /*
   * Constructor that appends to files by default
   */
  def this(outputFileUrl:String){
    this()

    this._outputFileUrl = outputFileUrl
    this._writer = new FileWriter(outputFileUrl, true);
  }

  def this(outputFileUrl:String, append:Boolean){
    this()

    this._outputFileUrl = outputFileUrl
    this._writer = new FileWriter(outputFileUrl, append);
    this._currentFunction = "Sys.init"
  }

  /*
   * Constructor that can specify if to append to a file instead of rewriting it.
   */
  def this(outputFileUrl:String, inputFileName:String, append:Boolean){
    this()

    this._outputFileUrl = outputFileUrl
    this._writer = new FileWriter(outputFileUrl, append);
    this._inputFileName = inputFileName
    this._currentFunction = inputFileName
  }

  /*
   * Writes the bootstrap code of the output .asm file
   */
  def writeInit():Unit ={
    append("// Initialize SP to 256")
    append("@256")
    append("D=A")
    append("@SP")
    append("M=D\n")

    writeCall("Sys.init", 0)
  }

  /*
   * Writes to the output file the assembly code
   * that implements the given arithmetic command.
   */
  def writeArithmetic(command:String):Unit ={

    if (command == "add"){
      writeAdd()
    }
    else if (command == "sub"){
      writeSub()
    }
    else if (command == "add"){
      writeAdd()
    }
    else if (command == "neg"){
      writeNeg()
    }
    else if (command == "eq"){
      writeEq()
    }
    else if (command == "gt"){
      writeGt()
    }
    else if (command == "lt"){
      writeLt()
    }
    else if (command == "not"){
      writeNot()
    }
    else if (command == "and"){
      writeAnd()
    }
    else if (command == "or"){
      writeOr()
    }
  }

  /*
   * Appends the hack assembly translation of the "push" instruction
   * into the output file.
   */
  def writePush(segment:String, index:Int):Unit ={
    append("// push " + segment + " " + index.toString)

    if (segment == "constant"){
      append("@" + index.toString)
      append("D=A")
      append("@SP")
      append("M=M+1")
      append("A=M-1")
      append("M=D\n")
    }
    else if (segment == "static") {
      append("@" + this._inputFileName + "." + index)
      append("D=M")
      append("@SP")
      append("M=M+1")
      append("A=M-1")
      append("M=D\n")
    }
    else {
      if (segment == "local"){
        append("@LCL")
        append("D=M")
      }
      else if (segment == "argument"){
        append("@ARG")
        append("D=M")
      }
      else if (segment == "this"){
        append("@THIS")
        append("D=M")
      }
      else if (segment == "that"){
        append("@THAT")
        append("D=M")
      }
      else if (segment == "temp"){
        append("@5")
        append("D=A")
      }
      else if(segment == "pointer"){
        append("@3")
        append("D=A")
      }
      else{
        writeErrorAndTerminate("ERROR: Unrecognized segment (writePush)")
      }

      append("@" + index.toString)
      append("A=A+D")
      append("D=M")
      append("@SP")
      append("M=M+1")
      append("A=M-1")
      append("M=D\n")
    }
  }

  /*
   * Appends the hack assembly translation of the "pop" instruction
   * into the output file.
   */
  def writePop(segment:String, index:Int):Unit ={
    append("// pop " + segment + " " + index.toString)

    if (segment == "static") {
      append("@" + _inputFileName + "." + index)
      append("D=A")
      append("@SP")
      append("M=M-1")
      append("A=M+1")
      append("M=D")
      append("A=A-1")
      append("D=M")
      append("A=A+1")
      append("A=M")
      append("M=D\n")
    }
    else{
      if (segment == "local"){
        append("@LCL")
        append("D=M")
      }
      else if (segment == "argument"){
        append("@ARG")
        append("D=M")
      }
      else if (segment == "this"){
        append("@THIS")
        append("D=M")
      }
      else if (segment == "that"){
        append("@THAT")
        append("D=M")
      }
      else if (segment == "temp"){
        append("@5")
        append("D=A")

        if (index > 7){
          writeErrorAndTerminate("ERROR: index of temp can't be greater than 7 (writePop.temp)")
        }
      }
      else if (segment == "pointer"){
        append("@3")
        append("D=A")
      }
      else{
        writeErrorAndTerminate("ERROR: Unrecognized segment (writePop)")
      }

      append("@" + index.toString)
      append("D=D+A")
      append("@SP")
      append("M=M-1")
      append("A=M+1")
      append("M=D")
      append("A=A-1")
      append("D=M")
      append("A=A+1")
      append("A=M")
      append("M=D\n")
    }

  }

  /*
   * Appends the hack assembly translation of the "goto" instruction
   * into the output file.
   */
  def writeGoto(label:String):Unit ={
    append("// goto " + label)
    append("@" + label)
    append("0;JMP\n")

    _ret += 1
  }

  /*
   * Appends the hack assembly translation of the "if-goto" instruction
   * into the output file.
   */
  def writeIf(label:String):Unit ={
    append("// if-goto " + label)
    append("@SP")
    append("M=M-1")
    append("A=M")
    append("D=M")
    append("@" + label)
    append("D;JNE\n")
  }

  /*
   * Appends the hack assembly translation of the "label" instruction
   * into the output file.
   */
  def writeLabel(label:String):Unit ={
    append("// label " + label)
    append("(" + label + ")\n")
  }

  /*
   * Appends the hack assembly translation of the "call" instruction
   * into the output file.
   */
  def writeCall(functionName:String, nArgs:Int):Unit ={
    if (nArgs < 0)
      writeErrorAndTerminate("nArgs of a function must be a non-negative number")

    val returnAddressLabel = _currentFunction + "$ret." + _call

    append("// *** call " + functionName + " " + nArgs + " ***")
    writePseudoPushLabel(returnAddressLabel)
    writePseudoPushSegment(SegmentType.LCL)
    writePseudoPushSegment(SegmentType.ARG)
    writePseudoPushSegment(SegmentType.THIS)
    writePseudoPushSegment(SegmentType.THAT)

    // ARG = SP - 5 - nArgs
    append("// \"ARG = SP - 5 - nArgs\" [nArgs = " + nArgs + "] (pseudo)")
    append("@SP")
    append("D=M")

    // Optimizations
    if (nArgs == 1){
      append("@6")
      append("D=D-A")
    }
    else{
      append("@5")
      append("D=D-A")
    }

    if (nArgs > 1){
      append("@" + nArgs)
      append("D=D-A")
    }

    append("@ARG")
    append("M=D\n")
    // END OF "ARG = SP - 5 - nArgs"

    // LCL = SP
    append("// \"LCL = SP\" (pseudo)")
    append("@SP")
    append("D=M")
    append("@LCL")
    append("M=D\n")

    // goto functionName
    writeGoto(functionName)
    writeLabel(returnAddressLabel)
    append("// *** end of \"call " + functionName + " " + nArgs + "\" ***\n")

    _call += 1
  }

  /*
   * Appends the hack assembly translation of the "function" instruction
   * into the output file.
   */
  def writeFunction(functionName:String, nVars:Int) ={
    _currentFunction = functionName

    append("// function " + _currentFunction + " " + nVars)
    writeLabel(_currentFunction)

    // "push 0" nVars times
    for (i <- 1 to nVars){
      writePush("constant", 0)
    }
  }

  /*
   * Appends the hack assembly translation of the "return" instruction
   * into the output file.
   */
  def writeReturn(): Unit ={
    append("// *** return ***")

    append("// endFrame = LCL (R14 = endFrame)")
    append("@LCL")
    append("D=M")
    append("@R14")
    append("M=D\n") // R14 = endFrame

    append("// retAddress = *(endFrame - 5)")
    append("@R14")
    append("D=M") // D = endFrame
    append("@5")
    append("A=D-A") // A = endFrame - 5
    append("D=M") // D = *(endFrame - 5)
    append("@R15")
    append("M=D\n") // R15 = retAddress

    append("// *ARG = pop()")
    append("@SP")
    append("A=M-1") // A = SP - 1
    append("D=M") // D = *(SP - 1) = retVal
    append("@SP")
    append("M=M-1") // SP--
    append("@ARG")
    append("A=M") // A = ARG
    append("M=D\n") // *ARG = retVal

    append("// SP = ARG + 1")
    append("@ARG")
    append("D=M")
    append("@SP")
    append("M=D+1\n")

    // seg = *(endFrame - i)
    // WHERE seg = (THAT || THIS || ARG || LCL) AND 1 <= i <= 4
    var i = 1
    List("THAT", "THIS", "ARG", "LCL").foreach(seg => {
      append("// " + seg + " = *(endFrame - " + i + ")")
      append("@R14")
      append("D=M") // D = endFrame

      // Optimization
      if (i == 1){
        append("A=D-1") // A = endFrame - 1
      }
      else{
        append("@" + i)
        append("A=D-A") // A = endFrame - i
      }

      append("D=M") // D = *(endFrame - i)
      append("@" + seg)
      append("M=D\n")

      i += 1
    })

    append("// goto retAddress")
    append("@R15")
    append("A=M") // A = retAddress
    append("0;JMP\n")
    append("// *** end of \"return\" ***\n")
  }

  /*
   * Sets the current input file's name
   */
  def setFileName(inputFileName:String): Unit ={
    this._inputFileName = inputFileName
  } // TODO: No ref. Delete?

  /*
   * Closes the output file
   */
  def close():Unit ={
    _writer.close()
  }


  // Helper Private Methods

  /*
   * Writes to the output file the assembly code
   * that implements the given command,
   * where command is either C_PUSH or C_POP
   *///TODO: no ref. delete?
  private def writePushPop(command:CommandType, segment:String, index:Int):Unit ={
    if (command == CommandType.C_PUSH){
      writePush(segment:String, index:Int)
    }
    else if (command == CommandType.C_POP){
      writePop(segment:String, index:Int)
    }
  }

  /*
   * Appends the hack assembly translation of the "add" instruction
   * into the output file.
   */
  private def writeAdd(): Unit ={
    append("// add")
    append("@SP")
    append("M=M-1")
    append("A=M")
    append("D=M")
    append("A=A-1")
    append("M=M+D\n")
  }

  /*
   * Appends the hack assembly translation of the "sub" instruction
   * into the output file.
   */
  private def writeSub(): Unit ={
    append("// sub")
    append("@SP")
    append("M=M-1")
    append("A=M")
    append("D=M")
    append("A=A-1")
    append("M=M-D\n")
  }

  /*
   * Appends the hack assembly translation of the "neg" instruction
   * into the output file.
   */
  private def writeNeg(): Unit ={
    append("// neg")
    append("@SP")
    append("A=M-1")
    append("M=-M\n")
  }

  /*
   * Appends the hack assembly translation of the "eq" instruction
   * into the output file.
   */
  private def writeEq(): Unit ={
    val label = this._currentFunction + "$JUMP." + _jmp

    append("// eq")
    append("@SP")
    append("M=M-1")
    append("A=M")
    append("D=M")
    append("A=A-1")
    append("D=M-D")
    append("M=-1")
    append("@" + label)
    append("D;JEQ")
    append("@SP")
    append("A=M-1")
    append("M=0")
    append("(" + label + ")\n")

    _jmp += 1
  }

  /*
   * Appends the hack assembly translation of the "gt" instruction
   * into the output file.
   */
  private def writeGt(): Unit ={
    val label = this._currentFunction + "$JUMP." + _jmp

    append("// gt")
    append("@SP")
    append("M=M-1")
    append("A=M")
    append("D=M")
    append("A=A-1")
    append("D=M-D")
    append("M=-1")
    append("@" + label)
    append("D;JGT")
    append("@SP")
    append("A=M-1")
    append("M=0")
    append("(" + label + ")\n")

    _jmp += 1
  }

  /*
   * Appends the hack assembly translation of the "lt" instruction
   * into the output file.
   */
  private def writeLt(): Unit ={
    val label = this._currentFunction + "$JUMP." + _jmp

    append("// lt")
    append("@SP")
    append("M=M-1")
    append("A=M")
    append("D=M")
    append("A=A-1")
    append("D=M-D")
    append("M=-1")
    append("@" + label)
    append("D;JLT")
    append("@SP")
    append("A=M-1")
    append("M=0")
    append("(" + label + ")\n")

    _jmp += 1
  }

  /*
   * Appends the hack assembly translation of the "not" instruction
   * into the output file.
   */
  private def writeNot(): Unit ={
    append("// not")
    append("@SP")
    append("A=M-1")
    append("M=!M\n")
  }

  /*
   * Appends the hack assembly translation of the "and" instruction
   * into the output file.
   */
  private def writeAnd(): Unit ={
    append("// and")
    append("@SP")
    append("M=M-1")
    append("A=M")
    append("D=M")
    append("A=A-1")
    append("M=D&M\n")
  }

  /*
   * Appends the hack assembly translation of the "or" instruction
   * into the output file.
   */
  private def writeOr(): Unit ={
    append("// or")
    append("@SP")
    append("M=M-1")
    append("A=M")
    append("D=M")
    append("A=A-1")
    append("M=D|M\n")
  }

  /*
   * Appends the hack assembly translation for the pseudo VM's
   * "push label" instruction.
   */
  private def writePseudoPushLabel(label:String): Unit ={
    append("// \"push " + label + "\" (pseudo)")
    append("@" + label)
    append("D=A")
    append("@SP")
    append("A=M")
    append("M=D")
    append("@SP")
    append("M=M+1\n")
  }

  /*
   * Appends the hack assembly translation for the pseudo
   * "push segment" instruction,
   * where segment can be LCL / ARG / THIS / THAT.
   */
  private def writePseudoPushSegment(segment:SegmentType): Unit ={
    append("// \"push " + segment + "\" (pseudo)")
    append("@SP")
    append("D=A")
    append("@" + segment)
    append("A=D+A")
    append("D=M")
    append("@SP")
    append("A=M")
    append("M=D")
    append("@SP")
    append("M=M+1\n")

    /*append("// \"push " + segment + "\" (pseudo)")
    append("@" + segment)
    append("D=A")
    append("@SP")
    append("A=M")
    append("M=D")
    append("@SP")
    append("M=M+1\n")*/
  }

  /*
   * Appends and error into the output file, and terminates the program.
   */
  private def writeErrorAndTerminate(errorMessage:String):Unit ={
    append("// " + errorMessage)
    close()
    throw new Exception(errorMessage)
  }

  /*
   * Appends a line string to the output file,
   * and appends '\n' to it.
   */
  private def append(line:String):Unit ={
    _writer.append(line + '\n');
  }
}
