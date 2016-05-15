package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import module.auth.AuthModule
import module.bank.BankModule

object BankController extends Controller {
    def queryBankAccount = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(BankModule.queryBankAccount)(false))
    def updateBankAccount = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(BankModule.updateBankAccount)(true))
}