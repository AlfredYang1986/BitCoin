package module.statistic

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.libs.concurrent._

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object StatisticModule {
    def statistics(data : JsValue) : JsValue = {  
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            ((from db() in "statistic").selectTop(1)("key")(x => x)).toList match {
              case Nil => DB2JsValue(createStatisticNode)
              case head :: Nil => DB2JsValue(head) 
              case _ => DB2JsValue(createStatisticNode)
            })))
    }
    
    def createStatisticNode : MongoDBObject = {
         val builder = MongoDBObject.newBuilder
        builder += "key" -> "ttt"
        builder += "user_increase_by_day" -> 0
        builder += "user_increase_by_month" -> 0
        builder += "user_increase_by_year" -> 0
        builder += "income_increase_by_day" -> 0
        builder += "income_increase_by_month" -> 0
        builder += "income_increase_by_year" -> 0

        builder.result
    }
    
    def DB2JsValue(x : MongoDBObject) : JsValue = {
        toJson(Map("user_increase_by_day" -> toJson(x.getAs[Number]("user_increase_by_day").get.intValue),
                   "user_increase_by_month" -> toJson(x.getAs[Number]("user_increase_by_month").get.intValue),
                   "user_increase_by_year" -> toJson(x.getAs[Number]("user_increase_by_year").get.intValue),
                   "income_increase_by_day" -> toJson(x.getAs[Number]("income_increase_by_day").get.floatValue),
                   "income_increase_by_month" -> toJson(x.getAs[Number]("income_increase_by_month").get.floatValue),
                   "income_increase_by_year" -> toJson(x.getAs[Number]("income_increase_by_year").get.floatValue)))
    }
    
    def pushNewUsers = {
        ((from db() in "statistic").selectTop(1)("key")(x => x)).toList match {
          case Nil => {
              val builder = createStatisticNode 
              builder += "user_increase_by_day" -> 1.asInstanceOf[Number]
              builder += "user_increase_by_month" -> 1.asInstanceOf[Number]
              builder += "user_increase_by_year" -> 1.asInstanceOf[Number]
              
              _data_connection.getCollection("statistic") += builder.result
          }
          case head :: Nil => {
              head += "user_increase_by_day" -> (head.getAs[Number]("user_increase_by_day").get.intValue + 1).asInstanceOf[Number]
              head += "user_increase_by_month" -> (head.getAs[Number]("user_increase_by_month").get.intValue + 1).asInstanceOf[Number]
              head += "user_increase_by_year" -> (head.getAs[Number]("user_increase_by_year").get.intValue + 1).asInstanceOf[Number]

              _data_connection.getCollection("statistic").update(DBObject("key" -> "ttt"), head)
          }
          case _ => Unit
        }
    }
    
    def pushIncome(amount : Float) = {
         ((from db() in "statistic").selectTop(1)("key")(x => x)).toList match {
          case Nil => {
              val builder = createStatisticNode
              builder += "income_increase_by_day" -> amount.asInstanceOf[Number]
              builder += "income_increase_by_month" -> amount.asInstanceOf[Number]
              builder += "income_increase_by_year" -> amount.asInstanceOf[Number]
              
              _data_connection.getCollection("statistic") += builder.result
          }
          case head :: Nil => {
              head += "income_increase_by_day" -> (head.getAs[Number]("income_increase_by_day").get.intValue + amount).asInstanceOf[Number]
              head += "income_increase_by_month" -> (head.getAs[Number]("income_increase_by_month").get.intValue + amount).asInstanceOf[Number]
              head += "income_increase_by_year" -> (head.getAs[Number]("income_increase_by_year").get.intValue + amount).asInstanceOf[Number]

              _data_connection.getCollection("statistic").update(DBObject("key" -> "ttt"), head)
          }
          case _ => Unit
        }
    }
}