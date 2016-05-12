package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import module.auth.AuthModule
import module.order.OrderModule

object OrderController extends Controller {
    def pushOrder = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(OrderModule.pushOrder)(true))
    def rejectOrder = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(OrderModule.rejectOrders)(true))
    def approveOrder = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(OrderModule.approveOrders)(true))

    def queryOrders = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(OrderModule.queryOrders)(true))
    def queryMyOrders = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(OrderModule.queryMyOrders)(true))
}