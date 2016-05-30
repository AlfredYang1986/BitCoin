package module.stoke

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
import module.common.http.{HTTP}
import module.statistic.StatisticModule.pushIncome

import module.auth.AuthModule

object CoinType {
  case object BTC extends CoinTypeDefines(0, "BTC")
  case object LTC extends CoinTypeDefines(1, "LTC")
}

sealed abstract class CoinTypeDefines(val s : Int, val des : String)

object StokeModule {
 
    object StokeModuleImpl {
        import CoinType._
        def checkCoinType(t : Int) : Boolean = 
            List(BTC, LTC).find (x => x.s == t) match {
              case None => false
              case Some(_) => true
            }
        
        def createCoin(data : JsValue) : MongoDBObject = {
            val coin_type = (data \ "type").asOpt[Int].map (x => x).getOrElse(-1)
            val coin_amount = (data \ "amount").asOpt[Float].map (x => x).getOrElse(0.0)
            
            val builder = MongoDBObject.newBuilder
            builder += "coin_type" -> coin_type
            builder += "coin_amount" -> coin_amount
            builder += "coin_total" -> coin_amount
            
            _data_connection.getCollection("stoke") += builder.result
            builder.result
        }
        
        def DB2JsValue(obj : MongoDBObject) : JsValue = 
            toJson(Map("coin_type" -> toJson(obj.getAs[Number]("coin_type").get.intValue),
                       "coin_name" -> toJson((obj.getAs[Number]("coin_type").get.intValue match {
                         case BTC.s => BTC.des
                         case LTC.s => LTC.des
                         case _ => "not suport"
                       })),
                       "coin_amount" -> toJson(obj.getAs[Number]("coin_amount").get.floatValue),
                       "coin_total" -> toJson(obj.getAs[Number]("coin_total").get.floatValue)))
      
        def queryStoke(t : Int) : Option[MongoDBObject] = {
            if (checkCoinType(t)) {
                (from db() in "stoke" where ("coin_type" -> t) select (x => x)).toList match {
                  case Nil => Some(createCoin(toJson(Map("type" -> toJson(t), "coin_amount" -> toJson(0)))))
                  case head :: Nil => Some(head)
                  case _ => None
                }
            } else None
        }
    }
 
    import CoinType._
    import StokeModule.StokeModuleImpl._
                   
    /**
     * admin auth
     */
    def pushStoke(user_id : String, data : JsValue) : JsValue = {
        if (AuthModule.adminAuthCheck(user_id)) {
            val coin_type = (data \ "type").asOpt[Int].map (x => x).getOrElse(-1)
            val coin_amount = (data \ "amount").asOpt[Float].map (x => x).getOrElse(0.0)
          
            if (!checkCoinType(coin_type)) ErrorCode.errorToJson("not support coin")
            else {
                (from db() in "stoke" where ("coin_type" -> coin_type) select (x => x)).toList match {
                  case Nil => toJson(Map("status" -> toJson("ok"), "result" -> DB2JsValue(createCoin(data))))
                  case head :: Nil => {
                      head += "coin_amount" -> (head.getAs[Number]("coin_amount").get.floatValue + coin_amount.asInstanceOf[Float]).asInstanceOf[Number]
                      head += "coin_total" -> (head.getAs[Number]("coin_total").get.floatValue + coin_amount.asInstanceOf[Float]).asInstanceOf[Number]
                      _data_connection.getCollection("stoke").update(DBObject("coin_type" -> coin_type), head)
                      toJson(Map("status" -> toJson("ok"), "result" -> DB2JsValue(head)))
                  }
                  case _ => ErrorCode.errorToJson("unknown error")
                }
            }
        } else ErrorCode.errorToJson("not have enough mana")
    }
    
    def popStoke(user_id : String, data : JsValue) : JsValue = {
        if (AuthModule.adminAuthCheck(user_id)) {
            val coin_type = (data \ "type").asOpt[Int].map (x => x).getOrElse(-1)
            val coin_amount = (data \ "amount").asOpt[Float].map (x => x).getOrElse(0.0)
            
            if (!checkCoinType(coin_type)) ErrorCode.errorToJson("not support coin")
            else {
                (from db() in "stoke" where ("coin_type" -> coin_type) select (x => x)).toList match {
                  case Nil => ErrorCode.errorToJson("not enough coin")
                  case head :: Nil => {
                      val tmp = head.getAs[Number]("coin_amount").get.floatValue - coin_amount.asInstanceOf[Float]
                      if (tmp < 0.0) ErrorCode.errorToJson("not enough coin")
                      else {
                          head += "coin_amount" -> tmp.asInstanceOf[Number]
                          head += "coin_total" -> (head.getAs[Number]("coin_total").get.floatValue - coin_amount.asInstanceOf[Float]).asInstanceOf[Number]
                          _data_connection.getCollection("stoke").update(DBObject("coin_type" -> coin_type), head)
                          toJson(Map("status" -> toJson("ok"), "result" -> DB2JsValue(head)))
                      }
                  }
                  case _ => ErrorCode.errorToJson("unknow error")
                }
            }
        } else ErrorCode.errorToJson("not have enough mana")
    }
     
    /**
     * user auth 
     */
    def queryAllStoke(user_id : String, data : JsValue) : JsValue = 
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            ((from db() in "stoke" select (x => DB2JsValue(x))).toList) match { 
                case Nil => {
                   (queryStoke(BTC.s) :: queryStoke(LTC.s) :: Nil) flatMap (x => x.map(DB2JsValue(_)))
                }
                case xls : List[JsValue] => xls
            }
        )))
    
        
    import module.trade.TradeModule.pushRecord
    import module.trade.RecordType._
    import module.account.AccountModule.AccountModuleImpl.{popMoneyImpl, queryAccount}
    
    def purchaseStoke(user_id : String, data : JsValue) : JsValue = {
        
        val amount = ((data \ "amount").asOpt[Float].map (x => x).getOrElse(0)).asInstanceOf[Float]
        val t = (data \ "type").asOpt[Int].map (x => x).getOrElse(-1)
        val price = ((data \ "price").asOpt[Float].map (x => x).getOrElse(0.0)).asInstanceOf[Float]
        val commition = amount * price * 0.01
       
        queryStoke(t) match {
          case None => ErrorCode.errorToJson("not support coin")
          case Some(stoke) => {
//              val p = if (t == BTC.s) HTTP("http://api.huobi.com/staticmarket/ticker_btc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty)
//                      else HTTP("http://api.huobi.com/staticmarket/ticker_ltc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty)
//              val price = ((p \ "ticker") \ "last").asOpt[Float].get

              val tmp = stoke.getAs[Number]("coin_amount").get.floatValue - amount.asInstanceOf[Float] 
              if (tmp < 0) ErrorCode.errorToJson("not enough coin")
              else {
                  queryAccount(user_id) match {
                    case None => ErrorCode.errorToJson("email not exist")
                    case Some(account) => {
                        val freeze = account.getAs[Number]("freeze").get.floatValue - price * amount.asInstanceOf[Float] - commition
                        if (freeze < 0) ErrorCode.errorToJson("not enough money")
                        else {
                            if (t == BTC.s) {
                               account += "btc" -> (account.getAs[Number]("btc").get.floatValue + amount.asInstanceOf[Float]).asInstanceOf[Number]
                               account += "freeze" -> freeze.asInstanceOf[Number]
                               
                               stoke += "coin_amount" -> tmp.asInstanceOf[Number]
                               
                               _data_connection.getCollection("accounts").update(DBObject("user_id" -> user_id), account)
                               _data_connection.getCollection("stoke").update(DBObject("coin_type" -> t), stoke)
                            
                               pushRecord(user_id, purchase.s, BTC.s, amount.asInstanceOf[Float], price * amount.asInstanceOf[Float])
                               pushIncome(commition.asInstanceOf[Float])
 
                               toJson(Map("status" -> toJson("ok"), "result" -> toJson("purchase success")))
                            }
                            else if (t == LTC.s) {
                               account += "ltc" -> (account.getAs[Number]("ltc").get.floatValue + amount.asInstanceOf[Float]).asInstanceOf[Number]
                               account += "freeze" -> freeze.asInstanceOf[Number]
                               
                               stoke += "coin_amount" -> tmp.asInstanceOf[Number]
                               
                               _data_connection.getCollection("accounts").update(DBObject("user_id" -> user_id), account)
                               _data_connection.getCollection("stoke").update(DBObject("coin_type" -> t), stoke)

                               pushRecord(user_id, purchase.s, LTC.s, amount.asInstanceOf[Float], price * amount.asInstanceOf[Float])
                               pushIncome(commition.asInstanceOf[Float])
                            
                               toJson(Map("status" -> toJson("ok"), "result" -> toJson("purchase success")))
                            }
                            else ErrorCode.errorToJson("not support coin")
                        }
                   }
                  }
              }
          }
        }
    }
    
    def sellMyStoke(user_id : String, data : JsValue) : JsValue = {
      
        val amount = ((data \ "amount").asOpt[Float].map (x => x).getOrElse(0)).asInstanceOf[Float]
        val t = (data \ "type").asOpt[Int].map (x => x).getOrElse(-1)
        val price = ((data \ "price").asOpt[Float].map (x => x).getOrElse(0.0)).asInstanceOf[Float]
        val commition = amount * price * 0.01
        
        queryStoke(t) match {
          case None => ErrorCode.errorToJson("not support coin")
          case Some(stoke) => {
//              val p = if (t == BTC.s) HTTP("http://api.huobi.com/staticmarket/ticker_btc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty)
//                      else HTTP("http://api.huobi.com/staticmarket/ticker_ltc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty)
//              val price = ((p \ "ticker") \ "last").asOpt[Float].get
                  
              queryAccount(user_id) match {
                  case None => ErrorCode.errorToJson("email not exist")
                  case Some(account) => {
                      t match {
                        case BTC.s => {
                            val tmp = account.getAs[Number]("btc").get.floatValue - amount.asInstanceOf[Float]
                            if (tmp < 0) ErrorCode.errorToJson("not enough coin")
                            else {
                                account += "btc" -> tmp.asInstanceOf[Number]
                                account += "balance" -> (account.getAs[Number]("balance").get.floatValue + price * amount.asInstanceOf[Float] - commition).asInstanceOf[Number]

                                stoke += "coin_amount" -> (stoke.getAs[Number]("coin_amount").get.floatValue + amount.asInstanceOf[Float]).asInstanceOf[Number]

                                _data_connection.getCollection("accounts").update(DBObject("user_id" -> user_id), account)
                                _data_connection.getCollection("stoke").update(DBObject("coin_type" -> t), stoke)

                                pushRecord(user_id, sell.s, BTC.s, amount.asInstanceOf[Float], price * amount.asInstanceOf[Float])
                                pushIncome(commition.asInstanceOf[Float])
                                toJson(Map("status" -> toJson("ok"), "result" -> toJson("sell success")))
                            }
                        }
                        case LTC.s => {
                            val tmp = account.getAs[Number]("ltc").get.floatValue - amount.asInstanceOf[Float]
                            if (tmp < 0) ErrorCode.errorToJson("not enough coin")
                            else {
                                account += "ltc" -> tmp.asInstanceOf[Number]
                                account += "balance" -> (account.getAs[Number]("balance").get.floatValue + price * amount.asInstanceOf[Float] - commition).asInstanceOf[Number]

                                stoke += "coin_amount" -> (stoke.getAs[Number]("coin_amount").get.floatValue + amount.asInstanceOf[Float]).asInstanceOf[Number]

                                _data_connection.getCollection("accounts").update(DBObject("user_id" -> user_id), account)
                                _data_connection.getCollection("stoke").update(DBObject("coin_type" -> t), stoke)

                                pushRecord(user_id, sell.s, LTC.s, amount.asInstanceOf[Float], price * amount.asInstanceOf[Float])
                                pushIncome(commition.asInstanceOf[Float])
                                toJson(Map("status" -> toJson("ok"), "result" -> toJson("sell success")))
                            }
                        }
                        case _ => ErrorCode.errorToJson("not support coin")
                      }
                 }
          }
        }
      }
    }
}