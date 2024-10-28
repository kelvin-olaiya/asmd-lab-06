package lab.u06

import PetriNet.*
import lab.u06.ReadersAndWriters.*
import lab.u06.utils.MSet

object KarpMiller extends App:

  type Infinity = Int
  given Infinity = Int.MaxValue
  enum Test:
    case A, B, C, D, E
  import Test.*

  val net: System[MSet[Test]] = PetriNet[Test](
    MSet() ~~> MSet(A, B),
    MSet(A) ~~> MSet(C),
    MSet(A, B, C) ~~> MSet(C)
  ).toSystem

  case class KMTree[T](
      node: Marking[T],
      children: Set[KMTree[T]] = Set.empty[KMTree[T]]
  )

  extension [T](m: Marking[T])
    infix def <(other: Marking[T]): Boolean =
      m.asMap.keySet.forall(t => m(t) <= other(t)) &&
        other.asMap.keySet.exists(t => m(t) < other(t))

    def widen(using
        infinity: Infinity = 10_000_000
    )(other: Marking[T]): Marking[T] =
      if m < other then
        MSet.ofMap(
          other.asMap.map((t, v) =>
            if m(t) < other(t) then (t, infinity) else (t, v)
          )
        )
      else m

  def treeFrom[T](
      system: System[MSet[T]]
  )(
      s0: Marking[T],
      path: List[Marking[T]] = List.empty[Marking[T]]
  ): KMTree[T] =
    println(s0)
    if (path.exists(p => s0 < p)) {
      return KMTree(s0)
    }

    def childrensOf(w: Marking[T]): Set[KMTree[T]] =
      system.next(w).map(m => treeFrom(system)(w widen s0, path :+ w))

    return KMTree(s0, childrensOf(s0))

  import State.*
  // println(MSet(A, B) widen MSet(A, B, C))
  println(treeFrom(net)(MSet()))
