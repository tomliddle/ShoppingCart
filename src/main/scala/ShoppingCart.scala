

abstract class StoreItem {
	val cost: BigDecimal
}

case class Apple() extends StoreItem {
	override val cost: BigDecimal = 0.6
}

case class Orange() extends StoreItem {
	override val cost: BigDecimal = 0.25
}

object ShoppingCart {
	def main(args: Array[String]): Unit = {
		val shoppingCart = new ShoppingCart
		shoppingCart.run
	}
}

class ShoppingCart {

	def run = {
		val list = List(Apple(), Apple(), Orange(), Apple())

		println(calculateCost(list))
	}

	def calculateCost(itemList: List[StoreItem]): BigDecimal = {
		itemList.map(_.cost).sum
	}
}
