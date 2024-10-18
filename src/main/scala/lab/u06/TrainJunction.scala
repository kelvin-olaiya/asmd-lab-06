package lab.u06

import lab.u06.utils.MSet
import lab.u06.PriorityPetriNet.~~>
import lab.u06.SystemAnalysis.pathsUpToDepth
import lab.u06.SystemAnalysis.paths

object TrainJunction:
  enum State:
    case T1, T2, T3, T1J, T2J, T3J, FJ, OS
  import State.*
  val network: System[MSet[State]] = PriorityPetriNet[State](
    MSet(T1, FJ) ~~> (3, MSet(T1J)),
    MSet(T2, FJ) ~~> (2, MSet(T2J)),
    MSet(T3, FJ) ~~> (1, MSet(T3J)),
    MSet(T1J) ~~> (3, MSet(OS, FJ)),
    MSet(T2J) ~~> (3, MSet(OS, FJ)),
    MSet(T3J) ~~> (3, MSet(OS, FJ))
  ).toSystem

object TryTrainJunction extends App:
  import TrainJunction.*
  import State.*
  for path <- network.pathsUpToDepth(MSet(T1, T2, T3, FJ), 10) do println(path)
