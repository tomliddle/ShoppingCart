
abstract class StoreItem {
	val cost: BigDecimal
	val dealGet: Int
	val dealPayFor: Int
}

case class Apple() extends StoreItem {
	override val cost: BigDecimal = 0.6
	override val dealGet = 2
	override val dealPayFor = 1
}

case class Orange() extends StoreItem {
	override val cost: BigDecimal = 0.25
	override val dealGet = 3
	override val dealPayFor = 2
}

class ShoppingCart {

	/**
	 * Step 1 answer - calculates cost without discount
	 */
	def calculateCost(itemList: List[StoreItem]): BigDecimal = itemList.map(_.cost).sum

	/**
	 * Step 2 answer - calculates cost with discount
	 */
	def calculateDiscountCost(itemList: List[StoreItem]): BigDecimal = {
		/**
		 * We assume here that all items in the list are from the same class.
		 */
		def costPerStoreItem(items: List[StoreItem]): BigDecimal = {
			if (items.isEmpty) 0
			else {
				//Remainder outside of deal
				val remainder = items.size % items.head.dealGet
				// Number of items needed to be bought for the max deals (excluding the remainder)
				val noNeededToBuy = (items.size - remainder) * items.head.dealPayFor / items.head.dealGet
				items.head.cost * (noNeededToBuy + remainder)
			}
		}

		itemList.groupBy(identity).map{x => costPerStoreItem(x._2)}.sum
	}

}
