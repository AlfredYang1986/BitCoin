
/**
 * 记录当前平台的交易量以及额度
 */

package controllers

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._
import module.trade.TradeModule
import module.auth.AuthModule

object TradeController extends Controller {
  	def queryTradeRecords = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(TradeModule.queryTradeRecords)(false))
  	def queryMyTradeRecords = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(TradeModule.queryUserTradeRecords)(false))
  	def queryBTCTradeRecords = Action (request => requestArgs(request)(TradeModule.queryBTCTradeRecords))
  	def queryLTCTradeRecords = Action (request => requestArgs(request)(TradeModule.queryLTCTradeRecords))
}