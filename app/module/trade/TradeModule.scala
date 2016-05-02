package module.trade

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.libs.concurrent._

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import java.util.Date

object RecordType {
  case object purchase extends RecordTypeDefines(0, "purchase")
  case object sell extends RecordTypeDefines(1, "sell")
}

sealed abstract class RecordTypeDefines(val s : Int, val des : String)

/**
 * 交易记录
 */
object TradeModule {
  
    def pushRecord(user_id : String, record_type : Int, coin_type : Int, amount : Float, value : Float) = {  
        val builder = MongoDBObject.newBuilder
        builder += "record_type" -> record_type
        builder += "coin_type" -> coin_type
        builder += "amount" -> amount
        builder += "value" -> value
        builder += "user_id" -> user_id
        builder += "date" -> new Date().getTime
      
        _data_connection.getCollection("records") += builder.result
    }
    
    def DB2JsValue(x : MongoDBObject) : JsValue =
        toJson(Map("owner_id" -> toJson(x.getAs[String]("user_id").get),
                   "coin_type" -> toJson(x.getAs[Number]("coin_type").get.intValue),
                   "amount" -> toJson(x.getAs[Number]("amount").get.floatValue),
                   "value" -> toJson(x.getAs[Number]("value").get.floatValue),
                   "record_type" -> toJson(x.getAs[Number]("record_type").get.intValue),
                   "date" -> toJson(x.getAs[Number]("date").get.longValue)))
 
    def queryTradeRecords(user_id : String, data : JsValue) : JsValue = {
        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(15)
        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
       
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "records").selectSkipTop(skip)(take)("date")(DB2JsValue(_)).toList)))
    }
    
    def queryUserTradeRecords(user_id : String, data : JsValue) : JsValue = {
        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(15)
        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(user_id)
       
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "records" where ("user_id" -> owner_id)).selectSkipTop(skip)(take)("date")(DB2JsValue(_)).toList)))
    }
    
    def queryBTCTradeRecords(data : JsValue) : JsValue = {
        import module.stoke.CoinType._
        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(15)
        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
       
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "records" where ("coin_type" -> BTC.s)).selectSkipTop(skip)(take)("date")(DB2JsValue(_)).toList)))
    }
    
    def queryLTCTradeRecords(data : JsValue) : JsValue = {
        import module.stoke.CoinType._
        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(15)
        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
       
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "records" where ("coin_type" -> LTC.s)).selectSkipTop(skip)(take)("date")(DB2JsValue(_)).toList)))
    }
}