
import fpinscala.state._
import fpinscala.state.RNG.Simple
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.{FlatSpec, Matchers}

class StateSuite extends FlatSpec with Matchers {

  private implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.01)

  val rng = Simple(911L)

  "nonNegativeInt()" should "return non-negative" in {
    val (i, n) = RNG.nonNegativeInt(rng)
    i should be > 0
  }

  "double()" should "return double in [0, 1)" in {
    val (d, _) = RNG.double(Simple(192))
    d should equal (0.5 +- 0.5)
  }

  "ints" should "return list of int and rnd" in {
    val count = 8
    val (ls, n) = RNG.ints(count)(Simple(100))
    val output = List(38474890, 419891633, 374484099, 288555289, 1438213179, 1601374765, -582853711, 1293446989)
    println(ls)
    ls should have size count
    ls shouldBe output
  }

  "nonNegLessThan" should "return random int less than n" in {
    val n = 100
    val output = RNG.nonNegLessThan(n)(Simple(-100))._1
    println(output)
    assert(output < n)
  }

  "simulateMachine with 10 coins, 5 candies" should "return 14 coins, 1 candies" in {
    val m = Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    State.simulateMachine(inputs).run(m)._1 shouldBe (1, 14)
  }
}
