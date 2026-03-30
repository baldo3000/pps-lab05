package it.unibo.pps.util

import it.unibo.pps.util.Optionals.Optional
import it.unibo.pps.util.Sequences.Sequence
import it.unibo.pps.util.Sequences.Sequence.*

import scala.annotation.tailrec

// Modified from the one seen during lesson
object Sets:
  object Set:
    opaque type Set[A] = Sequence[A]

    def apply[A](elements: A*): Set[A] = fromSequence(Sequence(elements *))

    def fromSequence[A](s: Sequence[A]): Set[A] = s match
      case Cons(h, t) => Cons(h, fromSequence(t.remove(h)))
      case Nil() => Nil()

    def intersection[A](s1: Set[A], s2: Set[A]): Set[A] = s1 match
      case Cons(h, t) if s2.contains(h) => Cons(h, intersection(t, s2.remove(h)))
      case Cons(_, t) => intersection(t, s2)
      case Nil() => Nil()

    extension [A](s: Set[A])
      def union(other: Set[A]): Set[A] = other match
        case Cons(h, t) => Cons(h, s.remove(h).union(t))
        case Nil() => s
      @tailrec
      def contains(a: A): Boolean = s match
        case Cons(h, t) if h == a => true
        case Cons(_, t) => t.contains(a)
        case Nil() => false
      def add(a: A): Set[A] = Set(a).union(s)
      def filter(f: A => Boolean): Set[A] = Sequence.filter(s)(f)
      def find(f: A => Boolean): Optional[A] = Sequence.find(s)(f)
      def map[B](f: A => B): Set[B] = Sequence.map(s)(f)
      def remove(a: A): Set[A] = s.filter(_ != a)
      def toSequence: Sequence[A] = s
