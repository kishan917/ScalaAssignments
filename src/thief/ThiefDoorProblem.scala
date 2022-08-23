package thief

import java.io.File
import scala.annotation.tailrec

object ThiefDoorProblem extends App {

  val source = scala.io.Source.fromFile(new File(this.getClass.getResource("thief_data.csv").toURI))
  val csvData = source.getLines

  val doorListCsv = csvData.toList.map(line => line.toCharArray.toList.map(c => c.toString.toInt))

  def process(doorList: List[Int]): Unit = {
    @tailrec
    def enterInDoor(doorList: List[Int], doorOpenCount: Int): Int = {
      if (doorList.isEmpty) doorOpenCount
      else if (doorList.head == 1) enterInDoor(doorList.tail, doorOpenCount)
      else enterInDoor(doorList.tail.map(x => (x + 1) % 2), doorOpenCount + 1)
    }
    println(enterInDoor(doorList, 0))
  }

  doorListCsv.foreach(process)

}
