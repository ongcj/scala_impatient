package JsonToCaseClass

import java.text.SimpleDateFormat

import JsonToCaseClass.Field.{ProfileField, TotalPurchaseField}
import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.util.Try

object JsonHelper {

  // we got a string in json format that we want to convert it to case class.
  // this can be done easily using json4s

  private def getFormat : DefaultFormats = {
    new DefaultFormats {
      // adjust accordingly to the source format
      override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    }
  }

  // implicit conversions from json string to ProfileField
  implicit def stringToProfileField(json: String): ProfileField = {
    implicit val formats: DefaultFormats = getFormat
    parse(json).extract[ProfileField]
  }

  def extractUserTotalCost(profile: ProfileField): TotalPurchaseField = {
    TotalPurchaseField(
      profile.userId,
      profile.country,
      Some(profile.purchases.map(_.cost.getOrElse(0.0)).sum)
    )
  }






}
