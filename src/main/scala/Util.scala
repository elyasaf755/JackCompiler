package Tools

import java.io.{File, FileWriter}

object Util {
  /*
   * Returns a list of all the files in a directory of some type (e.g - .txt, .vm, .asm, etc...)
   */
  def getFilesFromDir(dir:String, fileType:String):List[File] = {
    val d = new File(dir)

    if (d.exists && d.isDirectory) {
      return d.listFiles.filter(file =>{
        file.isFile && file.getName().endsWith("." + fileType)
      }).toList
    }
    else {
      return List[File]()
    }
  }

  /*
   * Clears the contents of a file
   */
  def clearFile(fileUrl:String):Unit ={
    val writer = new FileWriter(fileUrl, false);
    writer.close()
  }

  /*
   * Returns true if the string represents an Int value
   */
  def isInt(str:String):Boolean ={
    return str.toIntOption.isDefined
  }
}
