package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery.requestGetRequestArgs
import module.account.AccountModule
import module.auth.AuthModule

object accountController extends Controller {
    def createAccountWithToken = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AccountModule.queryAccount))
}