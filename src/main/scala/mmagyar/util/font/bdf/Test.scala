package mmagyar.util.font.bdf

import mmagyar.util.Bitmap

/** Magyar Máté 2017, all rights reserved */
object Test {

  def main(args: Array[String]): Unit = {

    val ldr  = new FontLoadBDFStd()
    val read = ldr.readBDF("fonts/ter-u18n.bdf")
//    val read = ldr.readBDF("test.bdf")

    val mgr = FontManager.readFileBDF(read, new Bitmap)
//    "szia".map(x=> mgr.types(x).toString + "\n").foreach(println)

    val arg = mgr('A')
    println("")


//    arg.lines.foreach(input=> {
//      (0 to 32).map(x => x < 32 && ((input & 1 << x) != 0)).foreach(x=> if(x) print('X') else print('.'))
//println("")
//    })
    arg.lines.foreach(input=> {
      println(input.toHexString)
    })
    println(mgr('A'))
  }
}
