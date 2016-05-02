package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import module.auth.AuthModule
import module.account.AccountModule

object AccountController extends Controller {
    def queryAccountInfo = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AccountModule.queryAccount)(true))
    def pushMoney = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AccountModule.pushMoney)(true))
    def popMoney = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AccountModule.popMoney)(true))
}