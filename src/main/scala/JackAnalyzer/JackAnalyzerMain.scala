import java.io.File

import Modules.JackAnalyzerClasses.{CompilationEngine, JackTokenizer}
import Tools.Util

object JackAnalyzerMain {
  def main(args: Array[String]): Unit = {

    var inputUrl:String = ""
    args.foreach(arg => {
      inputUrl += arg
    })

    inputUrl += "\\"

    writeTokensToFile(inputUrl)
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
}
