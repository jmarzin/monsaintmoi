package controllers

import javax.inject._
import models.MaDate
import org.joda.time.DateTime
import play.api.mvc.{MessagesRequest, _}

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(mcc: MessagesControllerComponents) extends MessagesAbstractController(mcc) {

  def index() = Action { implicit request: MessagesRequest[AnyContent] =>
    val maintenant = DateTime.now
    val aujourdhui = MaDate(maintenant.getYear, maintenant.getMonthOfYear, maintenant.getDayOfMonth)
    val retraite = MaDate(2018,7,1)
    val depart = MaDate(2019,5,11)
    val ecartRetraite = aujourdhui.ecartA(retraite)
    val ecartDepart = aujourdhui.ecartA(depart)
    Ok(views.html.base.index(ecartRetraite, ecartDepart))
  }}
