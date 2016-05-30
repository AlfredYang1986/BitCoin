package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import module.email.EmailModule

object EmailController extends Controller {
	  def send = Action (request => requestArgs(request)(EmailModule.sendEmail))
}