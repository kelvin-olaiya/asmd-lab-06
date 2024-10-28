package lab.u06

import org.scalacheck.Arbitrary.{arbitrary}
import lab.u06.PetriNet.Marking
import lab.u06.utils.MSet
import lab.u06.ReadersAndWriters.State.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.flatspec.AnyFlatSpec
import lab.u06.LTLPredicate.satisfies
import lab.u06.LTLPredicate.{*, given}

class ReadersAndWritersVerifier extends AnyPropSpec with Matchers:
  import ReadersAndWriters.*
  import SystemAnalysis.*

  val initialMarking = MSet(IDLE, IDLE, IDLE)
  val pathLength = 15
  val paths = network.pathsUpToDepth(initialMarking, pathLength)

  private def state(m: Marking[State]): Marking[State] => Boolean =
    (s: Marking[State]) => s.matches(m)

  property(
    s"In no path long at most $pathLength states the mutual exclusion property should fail"
  ):
    paths foreach: p =>
      p satisfies mutualExclusion shouldBe true

  property("If a reader wants to read it will eventually surely do it"):
    safeNetwork(3).paths(MSet(R, IDLE, IDLE), 13) foreach: p =>
      if !(p satisfies state(MSet(R)) -> F(state(MSet(RC))))
      then fail("Property failed on path: " + p)

  property("At most two writers"):
    val wrongStates = (Seq.fill(3)(WC1) ++ Seq.fill(3)(WC2)).combinations(3)
    atMostTwoWriters.pathsUpToDepth(MSet(W, W, W), 10) foreach: p =>
      if !(p satisfies G((s: Marking[State]) => !(wrongStates contains s)))
      then fail("Property failed on path: " + p)
