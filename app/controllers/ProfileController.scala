package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import module.auth.AuthModule

object ProfileController extends Controller {
  	def queryProfile = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AuthModule.queryProfile)(false))
	  def updateProfile = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AuthModule.updateProfile)(false))
}