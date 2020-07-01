import java.io.File

import Tools.Util

object Main {
  def main(args: Array[String]): Unit = {

    JackAnalyzerMain.main(args)

    var inputUrl:String = ""

    args.foreach(arg => {
      inputUrl += arg
    })

    val f = new File(inputUrl)

    if (f.isDirectory){
      VMTranslatorMain.main(args)
    }
    else if (f.isFile && f.getName.endsWith(".jack")){
      var newArgs:Array[String] = args

      newArgs(newArgs.length - 1) = newArgs(newArgs.length - 1).replace(".jack", ".vm")

      VMTranslatorMain.main(newArgs)
    }
    else{
      throw new Exception("The input path isn't a .vm file nor directory containing .vm files")
    }



  }

}
