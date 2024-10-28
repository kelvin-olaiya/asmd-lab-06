package lab.u06

import lab.u06.utils.MSet

object PriorityPetriNet:

  import PetriNet.{Marking, Trn}
  type PriorityPetriNet[P] = Set[(Int, Trn[P])]

  def apply[P](transitions: (Int, Trn[P])*): PriorityPetriNet[P] =
    transitions.toSet

  extension [P](pn: PriorityPetriNet[P])
    def toSystem: System[Marking[P]] = m =>
      val enabledTransitions = pn
        .filter { case (_, Trn(cond, _, inh)) =>
          (m disjoined inh) && (m matches cond)
        }
        .groupBy(_._1)
        .maxByOption(_._1)
        .map(_._2)
        .map(_.map(_._2))
        .getOrElse(Set())
      for
        Trn(cond, eff, _) <- enabledTransitions // get any transition
        out <- m extract cond // remove precondition
      yield out union eff // add effect

  extension [P](self: Marking[P])
    def ~~>(trn: (Int, Marking[P])) = trn match
      case (p, y) => (p, Trn(self, y, MSet()))

  extension [P](self: (Int, Trn[P]))
    def ^^^(z: Marking[P]) = self match
      case (p, Trn(c, e, _)) => (p, Trn(c, e, z))
