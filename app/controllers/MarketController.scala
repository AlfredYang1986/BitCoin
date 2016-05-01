package controllers
import play.api._
import play.api.mvc._

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import module.common.http._

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import akka.util.Timeout
import scala.concurrent.duration._

object MarketController extends Controller {
    def BTCMarket = Action {
        Ok(HTTP("http://api.huobi.com/staticmarket/ticker_btc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty))
    }
    
    def LTCMarket = Action {
        Ok(HTTP("http://api.huobi.com/staticmarket/ticker_ltc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty))
    }
    
    def CurrentMarket = Action {
        val f0 = Future(HTTP("http://api.huobi.com/staticmarket/ticker_btc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty))
    		val f1 = Future(HTTP("http://api.huobi.com/staticmarket/ticker_ltc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty))
    		
    		Ok(Await.result((f0 zip f1) map { x => 
    		  	toJson(Map("btc" -> toJson(x._1), "ltc" -> toJson(x._2)))
    		}, Timeout(2 second).duration).asInstanceOf[JsValue])
    }
    
    def BTCTrade = Action {
        Ok(HTTP("http://api.huobi.com/staticmarket/detail_btc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty))
    }

    def LTCTrade = Action {
        Ok(HTTP("http://api.huobi.com/staticmarket/detail_ltc_json.js").header("Accept" -> "application/json", "Content-Type" -> "application/json").get(Map.empty))
    }
}