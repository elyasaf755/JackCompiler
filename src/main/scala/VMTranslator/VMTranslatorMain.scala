import java.io.File

import Tools._

object VMTranslatorMain {
  def main(args: Array[String]): Unit = {
    var inputUrl:String = ""

    args.foreach(arg => {
      inputUrl += arg
    })

    inputUrl += "\\"

    val f = new File(inputUrl)

    if (f.isDirectory){
      val folderName = f.getName
      val outputFile = f.getPath + "\\" + folderName + ".asm"

      bootstrap(outputFile)

      this.getListOfVmFiles(inputUrl).foreach(file => {
        generateCode(outputFile, new Parser(file.getPath), true)
      })
    }
    else if (f.isFile){
      val outputPath = f.getPath
      generateCode(outputPath, new Parser(outputPath), false)
    }
    else{
      throw new Exception("Input path isn't a file nor directory")
    }
  }

  /*
   * Clears the file and initializes its SP (Bootstrap).
   * This should be called before translating a folder containing .vm files
   */
  private def bootstrap(outputFile:String): Unit ={
    val codeWriter = new CodeWriter(outputFile, false)
    codeWriter.writeInit()
    codeWriter.close()
  }

  private def generateCode(outputFileUrl:String, parser:Parser, append:Boolean): Unit ={
    val codeWriter = new CodeWriter(outputFileUrl, parser.getFileName(), append)

    while (parser.hasMoreCommands()) {
      parser.advance()

      write(parser, codeWriter)
    }

    codeWriter.close()
  }

  private def write(parser:Parser, codeWriter:CodeWriter):Unit ={
    val cmdType = parser.getCommandType()

    if (cmdType == CommandType.C_ARITHMETIC){
      codeWriter.writeArithmetic(parser.arg1())
    }
    else if (cmdType == CommandType.C_PUSH){
      codeWriter.writePush(parser.arg1(), parser.arg2())
    }
    else if (cmdType == CommandType.C_POP){
      codeWriter.writePop(parser.arg1(), parser.arg2())
    }
    else if (cmdType == CommandType.C_GOTO){
      codeWriter.writeGoto(parser.getCurrentFunction() + "$" + parser.arg1())
    }
    else if (cmdType == CommandType.C_IF){
      codeWriter.writeIf(parser.getCurrentFunction() + "$" + parser.arg1())
    }
    else if (cmdType == CommandType.C_LABEL){
      codeWriter.writeLabel(parser.getCurrentFunction() + "$" + parser.arg1())
    }
    else if (cmdType == CommandType.C_CALL){
      codeWriter.writeCall(parser.arg1(), parser.arg2())
    }
    else if (cmdType == CommandType.C_FUNCTION){
      codeWriter.writeFunction(parser.arg1(), parser.arg2())
    }
    else if (cmdType == CommandType.C_RETURN){
      codeWriter.writeReturn()
    }
  }

  /*
   * Returns a list of all the .vm files in a directory
   */
  private def getListOfVmFiles(dir: String):List[File] = {
    val d = new File(dir)

    if (d.exists && d.isDirectory) {
      return d.listFiles.filter(file =>{
        file.isFile && file.getName().endsWith(".vm")
      }).toList
    }
    else {
      return List[File]()
    }
  }
}
