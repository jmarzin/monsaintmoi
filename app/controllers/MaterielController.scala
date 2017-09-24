package controllers

import javax.inject._
import models.{Materiel, MaterielRep}
import play.api.mvc.{Action, _}
import play.api.data._
import play.api.data.Forms._
import play.api.libs.Files
import play.api.libs.Files.TemporaryFile

/**
 * Materiel form controller for Play Scala
 */
class MaterielController @Inject()(repo: MaterielRep, mcc: MessagesControllerComponents)
  extends MessagesAbstractController(mcc) with uploadfile {

  val materielForm = Form(
    mapping(
      "id" -> longNumber,
      "nom" -> nonEmptyText,
      "description" -> nonEmptyText,
      "photo" -> nonEmptyText,
      "poids" -> number(min = 1),
      "reforme" -> boolean
    )(Materiel.apply)(Materiel.unapply)
  )

  def listMateriel = Action { implicit request: MessagesRequest[AnyContent] =>
    val materiels = repo.listAll.sortWith(_.poids > _.poids)
    Ok(views.html.materiel.list(materiels))
  }

  def listTrekMateriel(idTrek: Long) = Action { implicit request: MessagesRequest[AnyContent] =>
    val materiels = repo.listTrekMateriel(idTrek).sortWith(_.poids > _.poids)
    Ok(views.html.materiel.list(materiels))
  }

  def showMateriel(id: Long) = Action { implicit request: MessagesRequest[AnyContent] =>
    repo.get(id).map { materiel =>
      Ok(views.html.materiel.detail(materiel))
    }.getOrElse(NotFound)
  }

  def deleteMateriel(id: Long) = Action { implicit request: MessagesRequest[AnyContent] =>
    if(request.session.get("admin").isEmpty) {
      Redirect(routes.MaterielController.showMateriel(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      repo.removeById(id)
      Redirect(routes.MaterielController.listMateriel()).flashing("success" -> "Matériel supprimé")
    }}

  def newMateriel = Action { implicit request: MessagesRequest[AnyContent] =>
    if(request.session.get("admin").isEmpty) {
      Redirect(routes.MaterielController.listMateriel()).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val form = if (request.flash.get("error").isDefined) {
        this.materielForm.bind(request.flash.data)
      } else {
        this.materielForm.fill(Materiel(0L, "", "", "0pasdimage.jpg", 0, reforme = false))
      }
      Ok(views.html.materiel.form(form))
    }}

  private def majNomPhoto(data: Map[String, Seq[String]], nouveauFichier: String): Map[String, Seq[String]] = {
    data.map{ case (key, values) =>
      if(key == "photo") (key, values.map(_ => nouveauFichier)) // trim whitespace from email
      else (key, values)
    }}

  private def traiteRetour(request: MessagesRequest[MultipartFormData[TemporaryFile]]): Form[Materiel] = {
    if (request.body.file("nouvellePhoto").get.filename.isEmpty) {
      this.materielForm.bindFromRequest(request.body.dataParts)
    } else {
      val cleanData = majNomPhoto(request.body.dataParts, uploadfile(request))
      this.materielForm.bindFromRequest(cleanData)
    }}

  def saveMateriel: Action[MultipartFormData[Files.TemporaryFile]] =
        Action(parse.multipartFormData) { implicit request: MessagesRequest[MultipartFormData[TemporaryFile]] =>
    if (request.session.get("admin").isEmpty) {
      Redirect(routes.MaterielController.listMateriel()).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      traiteRetour(request).fold(
        formWithErrors => {
          Redirect(routes.MaterielController.newMateriel()).flashing(Flash(formWithErrors.data) +
            ("error" -> "Erreur de saisie"))
        },
        materielData => {
          val id = repo.add(Materiel(materielData.id, materielData.nom, materielData.description,
            materielData.photo, materielData.poids, materielData.reforme))
          Redirect(routes.MaterielController.showMateriel(id))
            .flashing("success" -> ("Création réussie du matériel " + id.toString))
        })}}

  def editMateriel(id: Long) = Action { implicit request: MessagesRequest[AnyContent] =>
    if (request.session.get("admin").isEmpty) {
      Redirect(routes.MaterielController.listMateriel()).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      repo.get(id).map { materiel =>
        Ok(views.html.materiel.form(this.materielForm.fill(materiel), Option(id)))
      }.getOrElse(NotFound)
    }}

  def updateMateriel(id: Long): Action[MultipartFormData[Files.TemporaryFile]] =
      Action(parse.multipartFormData) { implicit request: MessagesRequest[MultipartFormData[TemporaryFile]] =>
    if (request.session.get("admin").isEmpty) {
      Redirect(routes.MaterielController.listMateriel()).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      traiteRetour(request).fold(
        hasErrors = _ => {
          Ok(views.html.materiel.form(materielForm, Option(id))).flashing(Flash(materielForm.data) +
            ("error" -> "Erreur de saisie"))
        },
        success = materielData => {
          repo.update(Materiel(materielData.id, materielData.nom, materielData.description, materielData.photo,
            materielData.poids, materielData.reforme))
          Redirect(routes.MaterielController.showMateriel(id)).flashing("success" -> ("Successful " + materielData.toString))
        })}}}
