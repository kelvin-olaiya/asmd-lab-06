package lab.u06

import lab.u06.utils.MSet
import lab.u06.PetriNet.~~>
import LTLPredicate.*
import lab.u06.PetriNet.Marking
import lab.u06.SystemAnalysis.Path

object ReadersAndWriters:
  enum State:
    case IDLE, R, W, DONE, RC, WC
  import State.*

  private def netStates(nTokens: Int): Set[Marking[State]] =
    State.values
      .flatMap(List.fill(nTokens)(_))
      .combinations(nTokens)
      .map(l => MSet(l*))
      .toSet

  def pathsUpTo(from: Marking[State], l: Int): LazyList[Path[Marking[State]]] =
    val states = netStates(from.size)
    def loop(s: Marking[State], depth: Int): LazyList[Path[Marking[State]]] =
      depth match {
        case 0 => LazyList()
        case 1 => LazyList(List(s))
        case _ =>
          for {
            path <- loop(s, depth - 1)
            next <- states.filter(s => network.next(path.last).contains(s))
          } yield path :+ next
      }
    loop(from, l)

  val noReaderAndWriterTogether: LTLPredicate[State] = `[]`(
    !_.matches(MSet(RC, WC))
  )
  val noMoreThan1Writer: LTLPredicate[State] = `[]`(!_.matches(MSet(WC, WC)))
  val mutualExclusion: LTLPredicate[State] =
    noReaderAndWriterTogether && noMoreThan1Writer

  val network: System[MSet[State]] = PetriNet[State](
    MSet(IDLE) ~~> MSet(R),
    MSet(IDLE) ~~> MSet(W),
    MSet(R) ~~> MSet(RC) ^^^ MSet(WC),
    MSet(W) ~~> MSet(WC) ^^^ MSet(WC, RC),
    MSet(RC) ~~> MSet(IDLE),
    MSet(WC) ~~> MSet(IDLE)
  ).toSystem

object TryPetriNet extends App:
  import ReadersAndWriters.*
  import State.*
  for path <- pathsUpTo(MSet(IDLE, IDLE, IDLE), 10) do println(path)
