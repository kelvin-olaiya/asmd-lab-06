package lab.u06

import lab.u06.utils.MSet
import javax.naming.Binding
import scala.reflect.ClassTag

object ColorPetriNet:
  type ColorPetriNet[L, T] = Set[Transition[L, T]]
  case class Token[T](color: T)

  type Expression = Token[?] => Seq[Binding]
  case class Arc[T](exp: Seq[Expression])
  case class Binding(variable: String, value: Any):
    def get[T: ClassTag]: Option[T] = summon[ClassTag[T]].unapply(value)

  type Place[T] = MSet[Token[T]]

  case class Transition[L, T](
      val conditions: Map[L, Arc[T]],
      val effect: Map[Arc[T], L]
  )

  def apply[L, T](transitions: Transition[L, T]*): ColorPetriNet[L, T] =
    transitions.toSet
