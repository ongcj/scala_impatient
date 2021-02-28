package JsonToCaseClass.Field

import java.sql.Timestamp

case class ProfileField(
                        userId : Option[String] = None,
                        userName: Option[String] = None,
                        createdAt: Option[Timestamp] = None,
                        updatedAt: Option[Timestamp] = None,
                        country: Option[String] = None,
                        purchases: Seq[PurchaseField] = Nil
                       ) {
  override def toString: String = {
    val sb = new StringBuilder
    sb.append(s"userId: $userId\n")
    sb.append(s"userName: $userName\n")
    sb.append(s"createdAt: $createdAt\n")
    sb.append(s"updatedAt: $updatedAt\n")
    sb.append(s"country: $country\n")
    sb.append(s"purchases: $purchases\n")
    sb.toString
  }
}
