package lab.u06

import lab.u06.utils.MSet
import lab.u06.PriorityPetriNet.{~~>, ###}
import lab.u06.SystemAnalysis.pathsUpToDepth
import lab.u06.SystemAnalysis.paths
import lab.u06.PriorityPetriNet.toSystem

object TrainJunction:
  enum State:
    case T1, T2, T3, T1J, T2J, T3J, FJ, OS
  import State.*
  val network: System[MSet[State]] = PriorityPetriNet[State](
    MSet(T1, FJ) ~~> MSet(T1J) ### 3,
    MSet(T2, FJ) ~~> MSet(T2J) ### 2,
    MSet(T3, FJ) ~~> MSet(T3J) ### 1,
    MSet(T1J) ~~> MSet(OS, FJ) ### 3,
    MSet(T2J) ~~> MSet(OS, FJ) ### 3,
    MSet(T3J) ~~> MSet(OS, FJ) ### 3
  ).toSystem

object TryTrainJunction extends App:
  import TrainJunction.*
  import State.*
  for path <- network.pathsUpToDepth(MSet(T1, T2, T3, FJ), 10) do println(path)
