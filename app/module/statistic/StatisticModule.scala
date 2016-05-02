package module.statistic

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.libs.concurrent._

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object StatisticModule {
    def statistics(data : JsValue) : JsValue = {  
        null
    }
}