package u04lab.polyglot.a01b
import u04lab.code

import scala.jdk.javaapi.OptionConverters
import u04lab.polyglot.OptionToOptional
import u04lab.code.Option
import u04lab.code.Option.*
import u04lab.code.List
import u04lab.code.Stream

import scala.annotation.tailrec
import scala.util.Random


/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private val mineSet: List[(Int, Int)] = appendRandom(List.empty, mines);
  private var selected: List[(Int, Int)] = List.empty;

  def hit(x: Int, y: Int): java.util.Optional[Integer] = List.contains(mineSet, (x,y)) match
    case false =>
      selected = List.append(selected, List.Cons((x,y), List.Nil()))
      OptionToOptional(Some(neighbours(x, y)))
    case _ => OptionToOptional(None())

  def won = List.length(selected) + List.length(mineSet) == size * size

  private def neighbours(x: Int, y: Int): Int =
    var total = 0
    for
      i <- -1 to 1
      j <- -1 to 1
    do
      total += List.length(List.filter(mineSet)(p => p == (x + i, y + j)))
    total

  @tailrec
  private def appendRandom(list: List[(Int, Int)], toGen: Int): List[(Int, Int)] = ((Random.nextInt(size), Random.nextInt(size)), toGen) match
    case (_,0) => list
    case (n, r) => if List.contains(list, n) then appendRandom(list, r) else appendRandom(List.append(List.Cons(n, List.Nil()), list), r - 1)



@main def mainWarehouse(): Unit =
  val x = new LogicsImpl(3, 2)



