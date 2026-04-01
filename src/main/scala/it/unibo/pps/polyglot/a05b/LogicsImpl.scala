package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.a05b.Logics

import scala.math.abs
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  private val random = Random()
  private val center = (x = random.nextInt(size - 2) + 1, y = random.nextInt(size - 2) + 1)
  private var ticks = 0

  override def tick(): Unit = ticks += 1

  override def isOver: Boolean = center.x - ticks < 0 || center.x + ticks >= size || center.y - ticks < 0 || center.y + ticks >= size

  override def hasElement(x: Int, y: Int): Boolean =
    abs(x - center.x) <= ticks && abs(y - center.y) <= ticks
