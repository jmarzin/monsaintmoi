package controllers

import java.math.BigInteger
import java.security.MessageDigest
import javax.inject._
import models.Admin
import play.Environment
import play.api.mvc.{Action, _}
import play.api.data._
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.Files
import play.api.libs.json.Json

/**
 * Admin form controller for Play Scala
 */

@Singleton
class AdminController @Inject()(mcc: MessagesControllerComponents) extends MessagesAbstractController(mcc) with uploadfile {

  val adminForm = Form(
    mapping(
      "password" -> nonEmptyText.verifying(
      "Mot de passe erroné ", f => String.format("%032X", new BigInteger(1, MessageDigest.getInstance("MD5").digest(f.getBytes()))) == "38B5188C3032225C37392EF863057344")
    )(Admin.apply)(Admin.unapply)
  )

  def adminGet() = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.admin.form(adminForm))
  }

  def adminPost() = Action { implicit request: MessagesRequest[AnyContent] =>
    adminForm.bindFromRequest.fold(
      formWithErrors => {
        // binding failure, you retrieve the form containing errors:
        Redirect(routes.AdminController.adminGet()).flashing(Flash(formWithErrors.data) +
            ("error" -> Messages("validation.errors")))
      },
      success = _ => {
        Redirect(routes.AdminController.adminGet()).flashing("success" -> "Vous êtes administrateur")
          .withSession(request.session - "admin" + ("admin" -> "true"))
      })}

  def agenda = Action {implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.admin.agenda())
  }

  def upLoadImage: Action[MultipartFormData[Files.TemporaryFile]] = Action(parse.multipartFormData) { implicit request =>
    val reponse = request.body.file("file").map { monFichier =>
      if (monFichier.filename.isEmpty) {
        "{}"
      } else {
        val fichierServeur = new java.io.File("%s/contenu/images/%s"
          .format(Environment.simple.rootPath.getCanonicalPath, monFichier.filename.replaceAll(" ","_")))
        """{"location" : "/assets/contenu/images/%s"}""".format(copyFile(fichierServeur, monFichier))
      }
    }.getOrElse("{}")
    Ok(Json.parse(reponse))
  }}
