package it.unibo.pps.polyglot.a01a

import it.unibo.pps.polyglot.a01a.Logics.Result
import it.unibo.pps.util.Sequences.Sequence.*
import it.unibo.pps.util.Sets.Set

import scala.util.Random

trait ScalaLogics:
  def hit(row: Int, col: Int): Result

object ScalaLogics:
  def apply(size: Int, boat: Int): ScalaLogics = ScalaLogicsImpl(size, boat)

  private class ScalaLogicsImpl(private val size: Int, private val boat: Int) extends ScalaLogics:

    import ScalaLogicsImpl.MAX_FAILURES

    private val random = Random()
    private val boatLeftEnd = Position(random.nextInt(size), random.nextInt(size - boat + 1))
    private var hits = Set.fromSequence(Nil[Position]())
    private var failures = 0

    private case class Position(x: Int, y: Int)

    def hit(row: Int, col: Int): Result =
      if !isBoat(row, col) then
        failures += 1
        if failures >= MAX_FAILURES then Result.LOST else Result.MISS
      else
        hits = hits.add(Position(row, col))
        if hits.size == boat then Result.WON else Result.HIT

    private def isBoat(row: Int, col: Int): Boolean =
      row == boatLeftEnd.x && col >= boatLeftEnd.y && col < (boatLeftEnd.y + boat)

  private object ScalaLogicsImpl:
    private val MAX_FAILURES = 5