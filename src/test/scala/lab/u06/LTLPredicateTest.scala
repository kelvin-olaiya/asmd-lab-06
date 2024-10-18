package lab.u06

import org.scalatest.funspec.AnyFunSpec
import lab.u06.LTLPredicate.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.infixOrderingOps

class LTLPredicateTest extends AnyFunSuite with Matchers:
  val path: List[Int] = List(1, 2, 3, 0, -1, 2, 3, 0, -2, 5)

  test("Test Next operator"):
    path satisfies X(p(_ == 2)) shouldBe true
    path satisfies X(X(X(p(_ == 0)))) shouldBe true
    path satisfies X((i: Int) => i <= 0) shouldBe false

  test("Test Globally operator"):
    path satisfies G(p(_ != 0)) shouldBe false
    path satisfies G(p(_.isInstanceOf[Int])) shouldBe true
    path satisfies G((i: Int) => i <= 5) shouldBe true

  test("Test Finally operator"):
    path satisfies F(p(_ == -1)) shouldBe true
    path satisfies F((i: Int) => i > 5) shouldBe false
    path satisfies F((i: Int) => i % 2 == 0) shouldBe true

  test("Implies operator"):
    val predicate = p(_ == 0) -> X(p(_ == -1))
    path satisfies predicate shouldBe true
    path satisfies `G`(predicate) shouldBe false

  test("Operators combination"):
    path satisfies p((i: Int) => i < 0) -> F(
      p((i: Int) => i == 5)
    ) shouldBe true
    path satisfies G(p((i: Int) => i > 0)) && F(
      p((i: Int) => i < 0)
    ) shouldBe false
    path satisfies G(p((i: Int) => i > 0)) || F(
      p((i: Int) => i < 0)
    ) shouldBe true
