package lab.u06

import lab.u06.utils.MSet

object PriorityPetriNet:

  import PetriNet.{Marking, Trn}
  case class PrioritizedTrn[P](priority: Int, trn: Trn[P])
  type PriorityPetriNet[P] = Set[PrioritizedTrn[P]]

  def apply[P](transitions: PrioritizedTrn[P]*): PriorityPetriNet[P] =
    transitions.toSet

  extension [P](pn: PriorityPetriNet[P])
    def toSystem: System[Marking[P]] = m =>
      val enabledTransitions = pn
        .filter { case PrioritizedTrn(_, trn) =>
          (m disjoined trn.inh) && (m matches trn.cond)
        }
        .groupBy(_.priority)
        .maxByOption(_._1)
        .map(_._2)
        .getOrElse(Set.empty[PrioritizedTrn[P]])
      for
        PrioritizedTrn(_, trn) <-
          enabledTransitions // get any transition
        out <- m extract trn.cond // remove precondition
      yield out union trn.eff // add effect

  extension [P](self: Marking[P])
    def ~~>(eff: Marking[P]) = PrioritizedTrn(0, Trn(self, eff, MSet()))

  extension [P](self: PrioritizedTrn[P])
    def ^^^(z: Marking[P]) = self.copy(trn = self.trn.copy(inh = z))

  extension [P](self: PrioritizedTrn[P])
    def ###(p: Int) = self.copy(priority = p)
