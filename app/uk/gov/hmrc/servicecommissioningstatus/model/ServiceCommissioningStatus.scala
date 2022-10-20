package uk.gov.hmrc.servicecommissioningstatus.model

import play.api.libs.json.Format

case class ServiceCommissioningStatus(
                                     hasRepo: Boolean
                                     )




object ServiceCommissioningStatus {

//  val versionFormat: Format[ServiceCommissioningStatus] =
//    Format.of[String].inmap(ServiceCommissioningStatus.apply, _.toString)

}