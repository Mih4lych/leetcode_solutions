package learning.fs2.udemy

import cats.effect.{IO, IOApp}

import java.io.{BufferedReader, FileReader}
import scala.collection.mutable.ListBuffer
import scala.reflect.io.File
import scala.util.Try

class Task01 extends IOApp.Simple {
  case class LegoSet(id: String, name: String, year: Int, themeId: Int, numParts: Int)

  def parseLegoSet(str: String): Option[LegoSet] = {
    val splitted = str.split(",")
    Try(LegoSet(
      id = splitted(0),
      name = splitted(1),
      year = splitted(2).toInt,
      themeId = splitted(3).toInt,
      numParts = splitted(4).toInt)).toOption
  }

  def readLegoSetImperative(fileName: String, p: LegoSet => Boolean, limit: Int): List[LegoSet] = {
    var reader: BufferedReader = null
    val legoSets: ListBuffer[LegoSet] = ListBuffer.empty
    var limitRem = limit

    try {
      reader = new BufferedReader(new FileReader(fileName))
      var line = reader.readLine()
      while (line != null && limitRem != 0) {
        val legoSet = parseLegoSet(line)
        legoSet.filter(p).foreach { ls =>
          legoSets.append(ls)
          limitRem -= 1
        }
        line = reader.readLine()
      }
    }
    finally {
      reader.close()
    }

    legoSets.toList
  }

  override def run: IO[Unit] = ???
}
