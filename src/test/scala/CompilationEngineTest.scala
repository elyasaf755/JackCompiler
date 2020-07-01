import java.io.{File, FileWriter}

import Modules.JackAnalyzerClasses.{CompilationEngine, JackTokenizer, Token, TokenType}
import Tools.Util
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer
import sys.process._
import scala.language.postfixOps

class CompilationEngineTest extends FunSuite{
  private var _i:Int = _

  private var _compareMode = true

  /* Project 11 Tests */

  test("Seven"){
    val dirUrl = "C:\\Users\\elyas\\IdeaProjects\\JackCompiler\\src\\test\\TestFiles\\Seven"
    compile(dirUrl)

    assert(compare(dirUrl))
  }

  test("ConvertToBin"){
    val dirUrl = "C:\\Users\\elyas\\IdeaProjects\\JackCompiler\\src\\test\\TestFiles\\ConvertToBin"
    compile(dirUrl)

    assert(compare(dirUrl))
  }

  test("Square"){
    val dirUrl = "C:\\Users\\elyas\\IdeaProjects\\JackCompiler\\src\\test\\TestFiles\\Square"
    compile(dirUrl)

    assert(compare(dirUrl))
  }

  test("Pong"){
    val dirUrl = "C:\\Users\\elyas\\IdeaProjects\\JackCompiler\\src\\test\\TestFiles\\Pong"
    compile(dirUrl)

    assert(compare(dirUrl))
  }

  test("Average"){
    val dirUrl = "C:\\Users\\elyas\\IdeaProjects\\JackCompiler\\src\\test\\TestFiles\\Average"
    compile(dirUrl)

    assert(compare(dirUrl))
  }

  test("ComplexArrays"){
    val dirUrl = "C:\\Users\\elyas\\IdeaProjects\\JackCompiler\\src\\test\\TestFiles\\ComplexArrays"
    compile(dirUrl)

    assert(compare(dirUrl))
  }

  test("Snake"){
    val dirUrl = "C:\\Users\\elyas\\Desktop\\Studies\\Snake"
    compile(dirUrl)

    assert(compare(dirUrl))
  }

  /* Project 10 Tests */
  /*
  test("GeneralPurposeTests"){
    val url = "C:\\Users\\elyas\\IdeaProjects\\JackCompiler\\src\\test\\TestSourceFiles"

    _i = 0;
    while(isTrue()){}
    assert(_i == 5)

    earlyRet()

    var pattern = "^[a-zA-Z_][\\w_ ]*".r
    assert(pattern.matches("_var1"))
    assert(!pattern.matches("1_var1"))

    pattern = "[^\n\"]*".r
    assert(pattern.matches("1asd#$35234fdwffwe_+="))
    assert(!pattern.matches("1asd#$3523\"4fdwffwe_+="))
    assert(!pattern.matches("1asd#$3523\n4fdwffwe_+="))
  }

  test("CompilationEngine.ArrayTest"){
    val inputUrl = "C:\\Users\\elyas\\IdeaProjects\\JackAnalyzer\\src\\test\\TestSourceFiles\\project 10 tests\\ArrayTest"
    writeTokensToFile(inputUrl)

    assert(compare(inputUrl))
  }

  test("CompilationEngine.ArrayTest.Main"){
    val inputUrl = "C:\\Users\\elyas\\IdeaProjects\\JackAnalyzer\\src\\test\\TestSourceFiles\\project 10 tests\\ArrayTest\\Main.jack"
    writeTokensToFile(inputUrl)

    assert(compare(inputUrl))
  }

  test("CompilationEngine.ExpressionLessSquare"){
    val inputUrl = "C:\\Users\\elyas\\IdeaProjects\\JackAnalyzer\\src\\test\\TestSourceFiles\\project 10 tests\\ExpressionLessSquare"
    writeTokensToFile(inputUrl)

    assert(compare(inputUrl))
  }

  test("CompilationEngine.ExpressionLessSquare.Main"){
    val inputUrl = "C:\\Users\\elyas\\IdeaProjects\\JackAnalyzer\\src\\test\\TestSourceFiles\\project 10 tests\\ExpressionLessSquare\\Main.jack"
    writeTokensToFile(inputUrl)

    assert(compare(inputUrl))
  }

  test("CompilationEngine.ExpressionLessSquare.Square"){
    val inputUrl = "C:\\Users\\elyas\\IdeaProjects\\JackAnalyzer\\src\\test\\TestSourceFiles\\project 10 tests\\ExpressionLessSquare\\Square.jack"
    writeTokensToFile(inputUrl)

    assert(compare(inputUrl))
  }

  test("CompilationEngine.ExpressionLessSquare.SquareGame"){
    val inputUrl = "C:\\Users\\elyas\\IdeaProjects\\JackAnalyzer\\src\\test\\TestSourceFiles\\project 10 tests\\ExpressionLessSquare\\SquareGame.jack"
    writeTokensToFile(inputUrl)

    assert(compare(inputUrl))
  }

  test("CompilationEngine.Square"){
    val inputUrl = "C:\\Users\\elyas\\IdeaProjects\\JackAnalyzer\\src\\test\\TestSourceFiles\\project 10 tests\\Square"
    writeTokensToFile(inputUrl)

    assert(compare(inputUrl))
  }

  test("CompilationEngine.Square.Main"){
    val inputUrl = "C:\\Users\\elyas\\IdeaProjects\\JackAnalyzer\\src\\test\\TestSourceFiles\\project 10 tests\\Square\\Main.jack"
    writeTokensToFile(inputUrl)

    assert(compare(inputUrl))
  }

  test("CompilationEngine.Square.Square"){
    val inputUrl = "C:\\Users\\elyas\\IdeaProjects\\JackAnalyzer\\src\\test\\TestSourceFiles\\project 10 tests\\Square\\Square.jack"
    writeTokensToFile(inputUrl)

    assert(compare(inputUrl))
  }

  test("CompilationEngine.SnakeGame"){
    val inputUrl = "C:\\Users\\elyas\\Desktop\\Studies\\Snake"
    writeTokensToFile(inputUrl)
  }

  test("CompilationEngine.isSymbolEqual"){
    val token = new Token(TokenType.SYMBOL, ";")

    assert(isSymbolEqual(token, ';'))
  }

   */



  /* Private Helper Methods */

  private def isTrue(): Boolean ={
    if (_i < 5){
      _i += 1

      return true
    }

    return false
  }

  private def isSymbolEqual(token:Token, symbol:Char): Boolean ={
    return token.tokenType == TokenType.SYMBOL &&
      token.tokenValue.length == 1 &&
      token.tokenValue(0) == symbol
  }

  private def earlyRet(): Unit ={
    return None
    if (true){
      assert(false)
    }
  }

  private def writeTokensToFile(inputUrl:String): Unit = {
    val f = new File(inputUrl)

    if (f.isDirectory){
      Util.getFilesFromDir(inputUrl,  "jack").foreach(file => {
        val inputFileUrl = file.getPath
        val outputFileUrl = inputFileUrl.replace(file.getName, "MY" + file.getName).replace(".jack", ".xml")
        val tokenizer = new JackTokenizer(inputFileUrl)

        // clears the current contents of the output file, if any
        Util.clearFile(outputFileUrl)

        val compilationEngine = new CompilationEngine(tokenizer, outputFileUrl)

        compilationEngine.compileClass()
      })
    }
    else if (f.isFile && f.getName.endsWith(".jack")){
      val inputFileUrl = f.getPath
      val outputFileUrl = inputFileUrl.replace(f.getName, "MY" + f.getName).replace(".jack", ".xml")
      val tokenizer = new JackTokenizer(inputFileUrl)

      // clears the current contents of the output file, if any
      Util.clearFile(outputFileUrl)

      val compilationEngine = new CompilationEngine(tokenizer, outputFileUrl)

      compilationEngine.compileClass()
    }
    else{
      throw new Exception("Input path isn't a .jack file nor directory containing .jack files")
    }
  }

  private def compile(inputUrl:String): Unit = {
    val f = new File(inputUrl)

    if (f.isDirectory){
      Util.getFilesFromDir(inputUrl,  "jack").foreach(file => {
        if (_compareMode){
          val inputFileUrl = file.getPath
          val outputFileUrl = inputFileUrl.replace(file.getName, "MY" + file.getName).replace(".jack", ".vm")
          val tokenizer = new JackTokenizer(inputFileUrl)

          // clears the current contents of the output file, if any
          Util.clearFile(outputFileUrl)

          val compilationEngine = new CompilationEngine(tokenizer, outputFileUrl)

          compilationEngine.compileClass()
        }
        else{
          val inputFileUrl = file.getPath
          val outputFileUrl = inputFileUrl.replace(".jack", ".vm")
          val tokenizer = new JackTokenizer(inputFileUrl)

          // clears the current contents of the output file, if any
          Util.clearFile(outputFileUrl)

          val compilationEngine = new CompilationEngine(tokenizer, outputFileUrl)

          compilationEngine.compileClass()
        }
      })
    }
    else if (f.isFile && f.getName.endsWith(".jack")){
      if (_compareMode){
        val inputFileUrl = f.getPath
        val outputFileUrl = inputFileUrl.replace(f.getName, "MY" + f.getName).replace(".jack", ".vm")
        val tokenizer = new JackTokenizer(inputFileUrl)

        // clears the current contents of the output file, if any
        Util.clearFile(outputFileUrl)

        val compilationEngine = new CompilationEngine(tokenizer, outputFileUrl)

        compilationEngine.compileClass()
      }
      else{
        val inputFileUrl = f.getPath
        val outputFileUrl = inputFileUrl.replace(".jack", ".vm")
        val tokenizer = new JackTokenizer(inputFileUrl)

        // clears the current contents of the output file, if any
        Util.clearFile(outputFileUrl)

        val compilationEngine = new CompilationEngine(tokenizer, outputFileUrl)

        compilationEngine.compileClass()
      }
    }
    else{
      throw new Exception("Input path isn't a .jack file nor directory containing .jack files")
    }
  }

  private def compare(inputUrl:String):Boolean ={
    if (!_compareMode){
      return true
    }

    val f = new File(inputUrl)
    val pythonScriptDirUrl = "C:\\Users\\elyas\\Documents\\Visual Studio Projects\\TextComparerHelper\\TextComparerHelper"

    if (f.isDirectory){
      val command = "cd " + pythonScriptDirUrl + " && TextComparerHelper.py " + "vm " + inputUrl
      val proc = stringToProcess("cmd /C " + command)
      val output = proc.!!
      println(output)
      return output.contains("*** SUCCESS! ALL ")
    }
    else if (f.isFile && f.getName.endsWith(".jack")){
      val inputDirUrl = f.getParentFile.getPath
      val command = "cd " + pythonScriptDirUrl + " && TextComparerHelper.py " + inputDirUrl
      val proc = stringToProcess("cmd /C " + command)
      val output = proc.!!
      println(output)
      return output.contains("*** SUCCESS! ALL ")
    }

    return false
  }
}
