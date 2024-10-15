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
    path satisfies o(p(_ == 2)) shouldBe true
    path satisfies o(o(o(p(_ == 0)))) shouldBe true
    path satisfies o((i: Int) => i <= 0) shouldBe false

  test("Test Always operator"):
    path satisfies `[]`(p(_ != 0)) shouldBe false
    path satisfies `[]`(p(_.isInstanceOf[Int])) shouldBe true
    path satisfies `[]`((i: Int) => i <= 5) shouldBe true

  test("Test Eventually operator"):
    path satisfies E(p(_ == -1)) shouldBe true
    path satisfies E((i: Int) => i > 5) shouldBe false
    path satisfies E((i: Int) => i % 2 == 0) shouldBe true

  test("Implies operator"):
    val predicate = p(_ == 0) -> o(p(_ == -1))
    path satisfies predicate shouldBe true
    path satisfies `[]`(predicate) shouldBe false

  test("Operators combination"):
    path satisfies p((i: Int) => i < 0) -> E(
      p((i: Int) => i == 5)
    ) shouldBe true
    path satisfies `[]`(p((i: Int) => i > 0)) && E(
      p((i: Int) => i < 0)
    ) shouldBe false
    path satisfies `[]`(p((i: Int) => i > 0)) || E(
      p((i: Int) => i < 0)
    ) shouldBe true
