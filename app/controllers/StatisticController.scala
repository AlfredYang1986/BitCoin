
/**
 * 平台统计数据
 */

package controllers

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._
import module.statistic.StatisticModule

object StatisticController extends Controller {
  	def statistics = Action (request => requestArgs(request)(StatisticModule.statistics))
}