import sun.security.ec.point.ProjectivePoint.Mutable
import scala.annotation.tailrec

object Main extends App {
  println("Task1")
  var dayOfTheWeek = List[String](
    "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday",
  )
  println("Task1a")
  var inscriptionDayOfTheWeek = ""
  for (day <- dayOfTheWeek) {
    inscriptionDayOfTheWeek += day
    if(dayOfTheWeek.last != day) {
      inscriptionDayOfTheWeek += ","
    }
  }
  println(inscriptionDayOfTheWeek)

  println("Task1b")
  inscriptionDayOfTheWeek = ""
  for(day <- dayOfTheWeek) {
    if(day.toLowerCase().startsWith("s")) {
      inscriptionDayOfTheWeek += day + ","
    }
  }
  inscriptionDayOfTheWeek = inscriptionDayOfTheWeek.dropRight(2)
  println(inscriptionDayOfTheWeek)

  println("Task1c")
  inscriptionDayOfTheWeek = ""
  var i = 0
  while (i < dayOfTheWeek.length) {
    inscriptionDayOfTheWeek += dayOfTheWeek(i) + ","
    i += 1
  }
  inscriptionDayOfTheWeek = inscriptionDayOfTheWeek.dropRight(2)
  println(inscriptionDayOfTheWeek)

  println("Task2")
  println("Task2a")
  inscriptionDayOfTheWeek = ""
  def createStringWithCommasFromList[T](list: List[T]): String = list match {
    case Nil => ""
    case _ => list.head + "," + createStringWithCommasFromList(list.tail)
  }
  inscriptionDayOfTheWeek = createStringWithCommasFromList(dayOfTheWeek).dropRight(2)
  println(inscriptionDayOfTheWeek)

  println("Task2b")
  inscriptionDayOfTheWeek = ""
  def createStringWithCommasFromListDesc[T](list: List[T]): String = list match {
    case Nil => ""
    case _ => createStringWithCommasFromListDesc(list.tail) + "," + list.head
  }
  inscriptionDayOfTheWeek = createStringWithCommasFromListDesc(dayOfTheWeek).drop(2)
  println(inscriptionDayOfTheWeek)

  println("Task3")
  inscriptionDayOfTheWeek = ""
  def createStringWithCommasFromListTail[T](list: List[T]): String = {
    @tailrec
    def appendStr[T](list: List[T], str: String): String = list match {
      case Nil => str.dropRight(2)
      case head :: tail => appendStr(tail, str + head + ",")
    }
    appendStr(list, "")
  }
  println(createStringWithCommasFromListTail(dayOfTheWeek))

  println("Task4")
  println("Task4a")
  def createStringWithCommasFromListFoldl(list: List[String]): String = {
    list.foldLeft(""){
      (acc, item) =>
        if(acc.isEmpty) acc + item
        else acc + "," + item
    }
  }
  println(createStringWithCommasFromListFoldl(dayOfTheWeek))

  println("Task4b")
  def createStringWithCommasFromListFoldr(list: List[String]): String = {
    list.foldRight(""){
      (acc, item) =>
        if(item.isEmpty) acc + item
        else acc + "," + item
    }
  }
  println(createStringWithCommasFromListFoldr(dayOfTheWeek))

  println("Task4c")
  def createStringWithCommasFromListFoldlOnlyP(list: List[String]): String = {
    list.filter(_.toLowerCase()
      .startsWith("s")
    ) .foldLeft(""){
      (acc, item) =>
        if(acc.isEmpty) acc + item
        else acc + "," + item
    }
  }
  println(createStringWithCommasFromListFoldlOnlyP(dayOfTheWeek))

  println("Task5")
  val productPrice = Map (
    "Chicken" -> 10,
    "Pork" -> 15,
    "Beef" -> 20,
    "Mutton" -> 25
  )
  println("Before change:")
  for(i <- productPrice) println(i._1 + "-" + i._2)
  println()
  val productPrice10 = productPrice.transform((_, v) => v * 0.9)
  println("Before change:")
  for(i <- productPrice10) println(i._1 + "-" + i._2)

  println("Task6")
  val tuple1 = ("Anton",21, 75)
  val tuple2 = ("A", Math.PI, true)
  def printTuple[A, B, C](tup: (A, B, C)): Unit = {
    println(tup._1 + " - " + tup._2 + " - " + tup._3)
  }
  printTuple(tuple1)
  printTuple(tuple2)

  println("Task7")
  val texts = List[String] ("one","2", "three","4","five")
  def toInt(s:String): Option[Int] = {
    try {
      Some(Integer.parseInt(s))
    }
    catch {
      case _: Exception => None
    }
  }
  for(n <- texts) println(toInt(n))

  println("Task8")
  def returnListWithNoZero(list: List[Int]): List[Int] = {
    @tailrec
    def accNewList(list: List[Int], listR: List[Int]): List[Int] = list match {
      case Nil => listR
      case head :: tail => {
        if (head == 0) accNewList(tail, listR)
        else accNewList(tail, listR.appended(head))
      }
    }
    accNewList(list, List.empty[Int])
  }
  var noZeros = ""
  for(wrt <- returnListWithNoZero(List[Int] (1, 2, 3, 0, 4, 0, 1))){
    noZeros += wrt + ", "
  }
  println(noZeros.dropRight(2))

  println("Task9")
  def increaseEveryElemByOne(list: List[Int]) = list.map(x => x + 1)
  var list = List[Int] (1, 2, 3, 4, 5, 6, 7)
  println("List before incrementing:" + list)
  list = increaseEveryElemByOne(list)
  println("List before incrementing:" + list)

  println("Task10")
  val realNumbers = List[Double](Math.PI, -Math.E, -10, -8, -6, -4, -1, 0, 2, 6, 10, 12, 15, 17)
  def returnListOfAbsValInRange(list: List[Double], r1: Int, r2: Int): List[Double] = {
    list.filter(x => x >= r1)
      .filter(x => x <= r2)
      .map(x => x.abs)
  }
  println("List before filtering: " + realNumbers)
  println("List after filtration: " + returnListOfAbsValInRange(realNumbers, -5, 12))

}
