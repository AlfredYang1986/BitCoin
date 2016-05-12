package module.timer

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor
import akka.actor.Inbox
import scala.concurrent.duration._

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import module.common.http.HTTP
import module.order.OrderModule.{ queryPurchaseBTC, queryPurchaseLTC, querySellBTC, querySellLTC, confirmOrder }

/**
 * messages for start schedule notification
 */
case object handleOrders

class TimerModule extends Actor {
	
	def receive = {
	  case handleOrders => this.handleAllOrders
	  case _ => Unit
	}
	
	def handleAllOrders = {
      val p_btc = HTTP("http://api.huobi.com/staticmarket/ticker_btc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty)
      val p_ltc = HTTP("http://api.huobi.com/staticmarket/ticker_ltc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty)
      val price_btc = ((p_btc \ "ticker") \ "last").asOpt[Float].get
      val price_ltc = ((p_ltc \ "ticker") \ "last").asOpt[Float].get
      
      (queryPurchaseBTC(price_btc) :: querySellBTC(price_btc) :: queryPurchaseLTC(price_ltc) :: querySellLTC(price_ltc) :: Nil).flatten foreach (x => confirmOrder(x))
	}
}