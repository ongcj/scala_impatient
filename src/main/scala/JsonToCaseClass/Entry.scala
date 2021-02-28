package JsonToCaseClass

import java.text.SimpleDateFormat

import JsonToCaseClass.Field.{ProfileField, PurchaseField}
import org.json4s._
import org.json4s.jackson.JsonMethods._


object Entry extends App {

  import JsonHelper._
  val jsonData =
    """
      |{
      |"userId": "123321",
      |"userName": "ongcj",
      |"createdAt": "2011-11-02 02:50:12",
      |"updatedAt": "2020-02-23 10:00:13",
      |"country": "Singapore",
      |"purchases": [
      |   {
      |     "orderId": "4321",
      |     "cost": 13.23,
      |     "coupon": "SUPERSALEDAY50",
      |     "status": "shipped",
      |     "createdAt": "2021-01-01 05:06:07",
      |     "updatedAt": "2021-02-01 08:01:02"
      |   },
      |   {
      |     "orderId": "4322",
      |     "cost": 13.21,
      |     "status": "delivered",
      |     "createdAt": "2021-01-01 05:06:07",
      |     "updatedAt": "2021-02-05 08:01:02"
      |   }
      |
      | ]
      |}
      |""".stripMargin

  // implicit convert json string to case class

  // accessing attribute in ProfileField
  println(jsonData.userName)

  // accessing seq of PurchaseField in ProfileField
  jsonData.purchases.foreach(println)

  // implicit convert json string to ProfileField for function call
  // function call returns new case class, TotalPurchaseField.
  println(extractUserTotalCost(jsonData))
}
