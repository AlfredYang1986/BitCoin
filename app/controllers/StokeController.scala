/**
 * 由于是平台交易，首先平台本身需要有比特币和莱特币的存量
 * 这个Controller只有管理员权限能访问，并由管理员直接输入
 * 到底输入的数值是多少，与本身有多少货币是无关的，
 * 管理员可以预先低价购买大量货币，在高价时放入平台中
 */

package controllers

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._
import module.auth.AuthModule
import module.stoke.StokeModule

object StokeController extends Controller {
    def pushStoke = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(StokeModule.pushStoke)(true))
    def popStoke = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(StokeModule.popStoke)(true))
    
    def queryStoke = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(StokeModule.queryAllStoke)(false))
    def purchaseStoke = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(StokeModule.purchaseStoke)(true))
    def sellStoke = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(StokeModule.sellMyStoke)(true))
}