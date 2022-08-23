package examdata

import java.io.File
import java.util.Scanner

object ExamDataProblem extends App {

  val source = scala.io.Source.fromFile(new File(this.getClass.getResource("exam_data.csv").toURI))
  val csvData = source.getLines
  csvData.foreach(line => {
    if (!line.isEmpty){
      val arr = line.split(",").map(_.trim).map(_.toInt)
      val logicFunct: (Int, Int, Int) => Unit = (K, L, M) => if (M >= L * K) println("Yes") else println("No")
      logicFunct(arr(0), arr(1), arr(2))
    }
  })
  source.close

}
