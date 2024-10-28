package lab.u06

import lab.u06.utils.MSet
import lab.u06.PetriNet.~~>
import LTLPredicate.*
import lab.u06.PetriNet.Marking
import lab.u06.SystemAnalysis.Path
import lab.u06.SystemAnalysis.pathsUpToDepth
import lab.u06.SystemAnalysis.paths

object ReadersAndWriters:
  enum State:
    case IDLE, R, W, DONE, RC, WC, WC1, WC2
  import State.*
  given [S]: Conversion[S => Boolean, LTLPredicate[S]] = Atom(_)

  val noReaderAndWriterTogether: LTLPredicate[Marking[State]] =
    G((m: Marking[State]) => !m.matches(MSet(RC, WC)))
  val noMoreThanOneWriter: LTLPredicate[Marking[State]] =
    G((m: Marking[State]) => !m.matches(MSet(WC, WC)))
  val mutualExclusion: LTLPredicate[Marking[State]] =
    noReaderAndWriterTogether && noMoreThanOneWriter

  val network: System[MSet[State]] = PetriNet[State](
    MSet(IDLE) ~~> MSet(R),
    MSet(IDLE) ~~> MSet(W),
    MSet(R) ~~> MSet(RC) ^^^ MSet(WC),
    MSet(W) ~~> MSet(WC) ^^^ MSet(WC, RC),
    MSet(RC) ~~> MSet(DONE),
    MSet(WC) ~~> MSet(DONE)
  ).toSystem

  private val safeNetworkPN = PetriNet[State](
    MSet(IDLE) ~~> MSet(R) ^^^ MSet(DONE),
    MSet(IDLE) ~~> MSet(W) ^^^ MSet(DONE),
    MSet(R) ~~> MSet(RC) ^^^ MSet(WC),
    MSet(W) ~~> MSet(WC) ^^^ MSet(WC, RC),
    MSet(RC) ~~> MSet(DONE),
    MSet(WC) ~~> MSet(DONE)
    // MSet(DONE) ~~> MSet(IDLE) ^^^ MSet(R, W, WC, RC)
  )

  def safeNetwork(nProcs: Int): System[MSet[State]] =
    (safeNetworkPN + (
      MSet.ofList(List.fill(nProcs)(DONE)) ~~>
        MSet.ofList(List.fill(nProcs)(IDLE))
    )).toSystem

  val atMostTwoWriters: System[MSet[State]] = PetriNet[State](
    MSet(IDLE) ~~> MSet(R),
    MSet(IDLE) ~~> MSet(W),
    MSet(R) ~~> MSet(RC) ^^^ MSet(WC1, WC2),
    MSet(W) ~~> MSet(WC1) ^^^ MSet(RC, WC1),
    MSet(W) ~~> MSet(WC2) ^^^ MSet(RC, WC2),
    MSet(RC) ~~> MSet(IDLE),
    MSet(WC1) ~~> MSet(IDLE),
    MSet(WC2) ~~> MSet(IDLE)
  ).toSystem

object TryReadersAndWriters extends App:
  import ReadersAndWriters.*
  import State.*

  safeNetwork(3).paths(MSet(IDLE, RC, IDLE), 15).take(3).foreach(println)
