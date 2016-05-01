package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery.{requestArgs}
import module.auth.AuthModule

object AuthController extends Controller {
	def register = Action (request => requestArgs(request)(AuthModule.register))
	def login = Action (request => requestArgs(request)(AuthModule.login))
}