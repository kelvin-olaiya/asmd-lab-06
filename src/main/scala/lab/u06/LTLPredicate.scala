package lab.u06

import lab.u06.SystemAnalysis.Path
import lab.u06.SystemAnalysis.pathsUpToDepth

trait LTLPredicate[S] extends (Path[S] => Boolean)

object LTLPredicate:

  given [S]: Conversion[S, Path[S]] = s => List(s)
  given [S]: Conversion[S => Boolean, LTLPredicate[S]] = Atom(_)

  case class Atom[S](f: S => Boolean) extends LTLPredicate[S]:
    override def apply(path: Path[S]): Boolean = f(path.head)

  case class And[S](lhs: LTLPredicate[S], rhs: LTLPredicate[S])
      extends LTLPredicate[S]:
    override def apply(path: Path[S]): Boolean = lhs(path) && rhs(path)

  case class Or[S](lhs: LTLPredicate[S], rhs: LTLPredicate[S])
      extends LTLPredicate[S]:
    override def apply(path: Path[S]): Boolean = lhs(path) || rhs(path)

  case class Not[S](p: LTLPredicate[S]) extends LTLPredicate[S]:
    override def apply(path: Path[S]): Boolean = !p(path)

  case class Always[S](p: LTLPredicate[S]) extends LTLPredicate[S]:
    override def apply(path: Path[S]): Boolean = path match
      case head :: next => p(path) && apply(next)
      case Nil          => true

  case class Eventually[S](p: LTLPredicate[S]) extends LTLPredicate[S]:
    override def apply(path: Path[S]): Boolean = path match
      case head :: next => p(path) || apply(next)
      case Nil          => false

  case class Next[S](p: LTLPredicate[S]) extends LTLPredicate[S]:
    override def apply(path: Path[S]): Boolean = path match
      case head :: next => p(next)
      case Nil          => false

  case class Implies[S](lhs: LTLPredicate[S], rhs: LTLPredicate[S])
      extends LTLPredicate[S]:
    override def apply(path: Path[S]): Boolean = path match
      case head :: tail if lhs(head) => rhs(path)
      case Nil                       => true
      case _                         => apply(path.tail)

  def p[S](pred: S => Boolean): LTLPredicate[S] = Atom(pred)
  def `[]`[S](f: LTLPredicate[S]): LTLPredicate[S] = Always(f)
  def `[]`[S](p: S => Boolean): LTLPredicate[S] = Always(Atom(p))
  def o[S](f: LTLPredicate[S]): LTLPredicate[S] = Next(f)
  def o[S](p: S => Boolean): LTLPredicate[S] = Next(Atom(p))
  def E[S](f: LTLPredicate[S]): LTLPredicate[S] = Eventually(f)
  def E[S](p: S => Boolean): LTLPredicate[S] = Eventually(Atom(p))

  extension [S](lhs: LTLPredicate[S])
    def unary_! : LTLPredicate[S] =
      Not(lhs)

    def &&(rhs: LTLPredicate[S]): LTLPredicate[S] =
      And(lhs, rhs)
    def ||(rhs: LTLPredicate[S]): LTLPredicate[S] =
      Or(lhs, rhs)
    def ->(rhs: LTLPredicate[S]): LTLPredicate[S] =
      Implies(lhs, rhs)

  def apply[S](f: Path[S] => Boolean): LTLPredicate[S] =
    f.asInstanceOf[LTLPredicate[S]]

  extension [S](path: Path[S])
    def satisfies(p: LTLPredicate[S]): Boolean = p(path)
