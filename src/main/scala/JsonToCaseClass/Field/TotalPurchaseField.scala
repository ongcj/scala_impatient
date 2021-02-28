package JsonToCaseClass.Field

case class TotalPurchaseField(
                              userId: Option[String] = None,
                              country: Option[String] = None,
                              totalCost: Option[Double] = None
                             ) {
  override def toString: String = {
    val sb = new StringBuilder
    sb.append(s"userId: $userId\n")
    sb.append(s"country: $country\n")
    sb.append(s"totalCost: $totalCost\n")
    sb.toString
  }
}
