package it.unibo.pps.ex

import it.unibo.pps.util.Sequences.Sequence
import it.unibo.pps.util.Sequences.Sequence.*

import scala.annotation.tailrec

object SameCategory:
  def unapplyNotTailRecursive(courses: Sequence[Course]): Option[String] = courses match
    case Cons(h, _) if courses.filter(_.category == h.category).size == courses.size => Some(h.category)
    case _ => None

  def unapply(courses: Sequence[Course]): Option[String] =
    @tailrec
    def _checkCommon(courses: Sequence[Course])(using category: String): Option[String] = courses match
      case Cons(course, t) => if course.category == category then _checkCommon(t) else None
      case _ => Some(category)

    courses match
      case Cons(course, _) => _checkCommon(courses)(using course.category)
      case _ => None

@main def testExtractor(): Unit =
  val scalaCourse = Course("SCALA01", "Functional Programming in Scala", "Prof. Odersky", "Programming")
  val pythonCourse = Course("PYTHON01", "Introduction to Python", "Prof. van Rossum", "Programming")
  val designCourse = Course("DESIGN01", "UI/UX Design Fundamentals", "Prof. Norman", "Design")
  val courses1 = Sequence(scalaCourse, pythonCourse, designCourse)
  val courses2 = Sequence(scalaCourse, pythonCourse)
  val courses3 = Sequence[Course]()

  def checkCategory(courses: Sequence[Course]): Unit = courses match
    case SameCategory(cat) => println(s"Courses have same category $cat ")
    case _ => println(s"Courses have different categories")

  checkCategory(courses1)
  checkCategory(courses2)
  checkCategory(courses3)
