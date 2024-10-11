package lab.u06

import lab.u06.utils.MSet
import lab.u06.PetriNet.~~>
import LTLPredicate.*

object ReadersAndWriters:
  enum State:
    case IDLE, R, W, DONE, RC, WC
  import State.*

  val noReaderAndWriterTogether: LTLPredicate[State] = `[]`(
    !_.matches(MSet(RC, WC))
  )
  val noMoreThan1Writer: LTLPredicate[State] = `[]`(
    !_.matches(MSet(WC, WC))
  )
  val mutualExclusion: LTLPredicate[State] =
    noReaderAndWriterTogether && noMoreThan1Writer

  val network: System[MSet[State]] = PetriNet[State](
    MSet(IDLE) ~~> MSet(R),
    MSet(IDLE) ~~> MSet(W),
    MSet(R) ~~> MSet(RC) ^^^ MSet(WC),
    MSet(W) ~~> MSet(WC) ^^^ MSet(WC, RC),
    MSet(RC) ~~> MSet(DONE),
    MSet(WC) ~~> MSet(DONE)
  ).toSystem
