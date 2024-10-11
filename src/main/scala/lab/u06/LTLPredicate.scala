package lab.u06

import lab.u06.SystemAnalysis.Path
import lab.u06.PetriNet.Marking

trait LTLPredicate[S] extends (Path[Marking[S]] => Boolean):
  def &&(other: LTLPredicate[S]): LTLPredicate[S] = s => apply(s) && other(s)
  def ||(other: LTLPredicate[S]): LTLPredicate[S] = s => apply(s) || other(s)

object LTLPredicate:
  def `[]`[S](f: Marking[S] => Boolean): LTLPredicate[S] = p => p.forall(f)
  def o[S](f: Marking[S] => Boolean): LTLPredicate[S] = p =>
    p.size > 1 && f(p(1))
  def F[S](f: Marking[S] => Boolean): LTLPredicate[S] = p => p.exists(f)

  def apply[S](f: Path[Marking[S]] => Boolean): LTLPredicate[S] =
    f.asInstanceOf[LTLPredicate[S]]
