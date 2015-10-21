import org.scalatest.BeforeAndAfterEach
import org.scalatest.Matchers
import org.scalatest.WordSpec

class ShoppingCartTest extends WordSpec with Matchers {

	val shoppingCart = new ShoppingCart

	"ShoppingCart" when {

		"calculating cost" should {

			"calculate 3 apples 1 orange" in {
				val list = List(Apple(), Apple(), Orange(), Apple())
				shoppingCart.calculateCost(list) should equal(BigDecimal(2.05))
			}

			"calculate 4 apples 4 oranges" in {
				val list = List(Apple(), Apple(), Orange(), Apple(), Apple(), Orange(), Orange(), Orange())
				shoppingCart.calculateCost(list) should equal(BigDecimal(3.4))
			}

			"calculate 1 orange" in {
				val list = List(Orange())
				shoppingCart.calculateCost(list) should equal(BigDecimal(0.25))
			}

			"calculate 1 apple" in {
				val list = List(Apple())
				shoppingCart.calculateCost(list) should equal(BigDecimal(0.60))
			}

			"calculate an empty list" in {
				val list = List()
				shoppingCart.calculateCost(list) should equal(BigDecimal(0))
			}
		}

		"calculating discount cost" should {

			"calculate 3 apples 1 orange" in {
				val list = List(Apple(), Apple(), Orange(), Apple())
				shoppingCart.calculateDiscountCost(list) should equal(BigDecimal(1.45))
			}

			"calculate 4 apples 4 oranges" in {
				val list = List(Apple(), Apple(), Orange(), Apple(), Apple(), Orange(), Orange(), Orange())
				shoppingCart.calculateDiscountCost(list) should equal(BigDecimal(1.95))
			}

			"calculate 1 orange" in {
				val list = List(Orange())
				shoppingCart.calculateDiscountCost(list) should equal(BigDecimal(0.25))
			}

			"calculate 1 apple" in {
				val list = List(Apple())
				shoppingCart.calculateDiscountCost(list) should equal(BigDecimal(0.60))
			}

			"calculate an empty list" in {
				val list = List()
				shoppingCart.calculateDiscountCost(list) should equal(BigDecimal(0))
			}
		}
	}
}
