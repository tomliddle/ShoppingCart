
class ShoppingCart {

	/**
	 * Step 1 answer - calculates cost without discount
	 */
	def calculateCost(itemList: List[String]): BigDecimal = {
		itemList.foldLeft(BigDecimal(0)) {
			(acc, curr) => acc + (curr match {
				case "Apple" => 0.6
				case "Orange" => 0.25
				case _ => 0
			})
		}
	}

	/**
	 * Step 2 answer - calculates cost with discount
	 */
	def calculateDiscountCost(itemList: List[String]): BigDecimal = {
		/**
		 * We assume here that all items in the list are from the same class.
		 */
		def itemsToBePurchased(size: Int, get: Int, payFor: Int): Int = {
			(size / get) * payFor + size % get
		}

		itemList.groupBy(x => x).foldLeft(BigDecimal(0)) {
			(acc, curr) => acc + (curr._1 match {
				case "Apple" => itemsToBePurchased(curr._2.length, 2, 1) * 0.6
				case "Orange" => itemsToBePurchased(curr._2.length, 3, 2) * 0.25
				case _ => 0
			})
		}
	}

}
