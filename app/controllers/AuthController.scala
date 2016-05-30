package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import module.auth.AuthModule

object AuthController extends Controller {
	def register = Action (request => requestArgs(request)(AuthModule.register))
	def login = Action (request => requestArgs(request)(AuthModule.login))
	def admainLogin = Action (request => requestArgs(request)(AuthModule.admainLogin))
  def changePwd = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AuthModule.updateProfile)(false))
  def changeTradePwd = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AuthModule.updateProfile)(false))
  def pushBankAccount = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AuthModule.pushBankAccount)(true))
  def popBankAccount = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AuthModule.popBankAccount)(true))
}