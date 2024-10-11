package lab.u06

import org.scalacheck.Properties
import org.scalacheck.Arbitrary.{arbitrary}
import lab.u06.PetriNet.Marking
import lab.u06.utils.MSet
import lab.u06.ReadersAndWriters.State.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{forAll, propBoolean}

object ReadersAndWritersVerifier extends Properties("ReadersAndWriters"):
  import ReadersAndWriters.*
  import SystemAnalysis.*

  given Arbitrary[Marking[State]] = Arbitrary:
    for
      n <- Gen.choose(1, 3)
      states <- Gen.listOfN(n, Gen.const(IDLE))
    yield MSet(states*)

  def pathsGenerator(length: Int = 10): Gen[Path[Marking[State]]] =
    for
      initialMarking <- arbitrary[Marking[State]]
      p <- Gen.oneOf(network.pathsUpToDepth(initialMarking, length))
    yield p

  given Arbitrary[Path[Marking[State]]] = Arbitrary(pathsGenerator(60))

  import LTLPredicate.*
  property("In no path long at most 60 states mutual exclusion fails") = forAll:
    (path: Path[Marking[State]]) =>
      ("No readers and writers toghether" |: noReaderAndWriterTogether(path)) &&
        ("No more than 1 writer" |: noMoreThan1Writer(path))
