import org.scalatest.BeforeAndAfterEach
import org.scalatest.Matchers
import org.scalatest.WordSpec

class ShoppingCartTest extends WordSpec with Matchers with BeforeAndAfterEach {

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

			"calculate an empty list" in {
				val list = List()
				shoppingCart.calculateCost(list) should equal(BigDecimal(0))
			}
		}
	}
}