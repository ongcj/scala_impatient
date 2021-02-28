package JsonToCaseClass.Field

import java.sql.Timestamp

case class PurchaseField(
                          orderId: Option[String] = None,
                          cost: Option[Double] = None,
                          coupon: Option[String] = None,
                          status: Option[String] = None,
                          createdAt: Option[Timestamp] = None,
                          updatedAt: Option[Timestamp] = None
                        ) {
  override def toString: String = {
    val sb = new StringBuilder
    sb.append(s"orderId: $orderId\n")
    sb.append(s"cost: $cost\n")
    sb.append(s"coupon: $coupon\n")
    sb.append(s"status: $status\n")
    sb.append(s"createdAt: $createdAt\n")
    sb.append(s"updatedAt: $updatedAt\n")
    sb.toString
  }
}
