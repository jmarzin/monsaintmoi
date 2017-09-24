package controllers

import javax.inject._
import models._
import play.Environment
import play.api.mvc.{Action, _}
import play.api.data._
import play.api.data.Forms._
import play.api.libs.Files
import play.api.libs.Files.TemporaryFile

case class GpxForm(id: Long, idTrek: Long, titre: String, sousTitre: String, description: String, listeMatos: String, nomFichier: String, altitudeMinimum: Int, altitudeMaximum: Int, ascensionTotale: Int, descenteTotale: Int,
                   heureDebut: String, heureFin: String, distanceTotale: Int, depart: String,
                   arrivee: String, coordonneesPix: String, typegpx: String, listeFichiers: Seq[String], listeMaterielsTreks: Seq[String])

/**
 * Gpx form controller for Play Scala
 */
class GpxController @Inject()(repoGpx: GpxRep,
                              repoTrekMateriels: TrekMaterielRep,
                              repoMateriels: MaterielRep,
                              mcc: MessagesControllerComponents) extends MessagesAbstractController(mcc)
                                                                 with uploadfile {

  val gpxForm = Form(
    mapping(
      "id" -> longNumber,
      "idTrek" -> longNumber,
      "titre" -> nonEmptyText,
      "sousTitre" -> nonEmptyText,
      "description" -> text,
      "listeMatos" -> text,
      "nomFichier" -> text,
      "altitudeMinimum" -> number,
      "altitudeMaximum" -> number,
      "ascensionTotale" -> number,
      "descenteTotale" -> number,
      "heureDebut" -> text,
      "heureFin" -> text,
      "distanceTotale" -> number,
      "depart" -> text,
      "arrivee" -> text,
      "coordonneesPix" -> text,
      "typegpx" -> text,
      "listeFichiers" -> seq(text),
      "listeMaterielsTrek" -> seq(text)
    )(GpxForm.apply)(GpxForm.unapply)
  )

  val taillePage = 20

  def apropos = Action { implicit request: MessagesRequest[AnyContent] =>
    repoGpx.get(1L).map { gpx =>
      val existeListeMateriels = repoTrekMateriels.exist(gpx.id)
      Ok(views.html.gpx.details(gpx, existeListeMateriels))
    }.getOrElse(NotFound)
  }

  private def preparePage(gpxs: Seq[Gpx], page: Int): (Seq[Gpx], Int, Int) = {
    val nbPages = Math.ceil(gpxs.size/taillePage.toFloat).toInt
    val pageAAfficher = (page max 1) min nbPages
    val gpxAAffichier = gpxs.drop(taillePage * (pageAAfficher - 1)).slice(0, taillePage)
    (gpxAAffichier, pageAAfficher, nbPages)
  }

  def listGpx(typegpx: String, page: Int) = Action { implicit request: MessagesRequest[AnyContent] =>
    val gpxs = repoGpx.listAll(typegpx).sortWith(_.heureDebut > _.heureDebut)
    val gpxsAAfficher = preparePage(gpxs, page)
    Ok(views.html.gpx.list(gpxsAAfficher._1, typegpx, gpxsAAfficher._2, gpxsAAfficher._3))
  }

  def listGpxTrk(id: Long, page: Int) = Action { implicit request: MessagesRequest[AnyContent] =>
    val gpxs = repoGpx.listByTrekId(id).sortWith(_.heureDebut < _.heureDebut)
    val gpxsAAfficher = preparePage(gpxs, page)
    Ok(views.html.gpx.list(gpxsAAfficher._1, "R", gpxsAAfficher._2, gpxsAAfficher._3))
  }

  def showGpx(id: Long) = Action { implicit request: MessagesRequest[AnyContent] =>
    repoGpx.get(id).map { gpx =>
      val existeListeMateriels = repoTrekMateriels.exist(gpx.id)
      Ok(views.html.gpx.details(gpx, existeListeMateriels))
    }.getOrElse(NotFound)
  }

  def deleteGpx(id: Long) = Action { implicit request: MessagesRequest[AnyContent] =>
    if(request.session.get("admin").isEmpty) {
      Redirect(routes.GpxController.showGpx(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      repoGpx.get(id).map { gpx =>
        repoGpx.removeById(id)
        Redirect(routes.GpxController.listGpx(gpx.typegpx, 1)).flashing("success" -> "Tracé supprimé")
      }.getOrElse(NotFound)
    }}

  def newGpx(typegpx: String) = Action { implicit request: MessagesRequest[AnyContent] =>
    if(request.session.get("admin").isEmpty) {
      Redirect(routes.GpxController.listGpx(typegpx, 1)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val tableaux = if(typegpx == "R") {
        val liste = ("" +: new java.io.File("%s/contenu/gpx/randos/".
          format(Environment.simple.rootPath.getCanonicalPath)).
          listFiles.map(f => f.getName).filter(_.toLowerCase.endsWith(".gpx")).
          toSeq).diff(repoGpx.listGpxFiles)
        (liste.map((0L, _, 0L)), Seq[Materiel]())
      } else {
        (repoGpx.listCandidatTrek(0L).sortWith(_._2 < _._2), repoMateriels.listAll.filter(!_.reforme))
      }
      val form = if (request.flash.get("error").isDefined) {
        val errorForm = this.gpxForm.bind(request.flash.data)
        errorForm
      } else {
        this.gpxForm.fill(GpxForm(0L, 0L, "", "", "", "", "", 0, 0, 0, 0, "", "", 0, "", "", "", typegpx,
          Seq(), Seq()))
      }
      Ok(views.html.gpx.form(form,
        listeCandidats = tableaux._1,
        listeMateriels = tableaux._2))
    }
  }

  private def majNomGpx(data: Map[String, Seq[String]], nouveauFichier: String): Map[String, Seq[String]] = {
    data.map{ case (key, values) =>
      if(key == "nomFichier") (key, values.map(_ => nouveauFichier)) // trim whitespace from email
      else (key, values)
    }}

  private def traiteRetour(request: MessagesRequest[MultipartFormData[TemporaryFile]]): Form[GpxForm] = {
    if (request.body.file("nouveauGpx").isEmpty || request.body.file("nouveauGpx").get.filename.isEmpty) {
      this.gpxForm.bindFromRequest(request.body.dataParts)
    } else {
      val cleanData = majNomGpx(request.body.dataParts, uploadfile(request))
      this.gpxForm.bindFromRequest(cleanData)
    }}


  def saveGpx: Action[MultipartFormData[Files.TemporaryFile]] =
    Action(parse.multipartFormData) { implicit request: MessagesRequest[MultipartFormData[TemporaryFile]] =>
    //au retour, nomFichier contient gpx.nomFichier
    //           listeFichier le ou  les fichiers candidats du serveur
    //           nouveauGpx le fichier local
    val typegpx = request.body.dataParts("typegpx").head
    if (request.session.get("admin").isEmpty) {
      Redirect(routes.GpxController.listGpx(typegpx, 1)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      traiteRetour(request).fold(
        formWithErrors => {
          Redirect(routes.GpxController.newGpx(typegpx)).flashing(Flash(formWithErrors.data) +
            ("error" -> "Erreur de saisie"))
        },
        gpx => {
          var idx = 0L
          if(typegpx == "R") {
            if(request.body.file("nouveauGpx").get.filename.isEmpty) {
              idx = repoGpx.add(Gpx(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos,
                gpx.nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale, gpx.descenteTotale,
                gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart, gpx.arrivee, gpx.coordonneesPix,
                gpx.typegpx))
            } else {
              idx = repoGpx.add(Gpx.creer("R", gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description,
                gpx.listeMatos, gpx.nomFichier))
            }
          } else {
            val nextIndex = repoGpx.lastValueIndex + 1
            val listeFichiers = for(f <- gpx.listeFichiers) yield {
              val sub = f.replaceAll("[\\(\\)]","").split(",")
              (sub(0),sub(1))
            }
            val listeMaterielsARattacher = for(f <- gpx.listeMaterielsTreks) yield {
              val sub = f.replaceAll("[\\(\\)]","").split(",")
              sub(0).toLong
            }
            Gpx.fusion(listeFichiers.map(_._2), nextIndex.toString + ".gpx")
            idx = repoGpx.add(Gpx.creer("T", gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre,
              gpx.description, gpx.listeMatos, nextIndex.toString + ".gpx"))
            repoGpx.rattacheGpx(idx, listeFichiers.map(_._1.toLong))
            repoTrekMateriels.maj(idx, listeMaterielsARattacher)
          }
          Redirect(routes.GpxController.showGpx(idx))
            .flashing("success" -> ("Création réussie du tracé " + idx.toString))
        })}}


  def editGpx(id: Long) = Action { implicit request: MessagesRequest[AnyContent] =>
    if (request.session.get("admin").isEmpty) {
      Redirect(routes.GpxController.showGpx(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      repoGpx.get(id).map { gpx =>
        val tableaux = if(gpx.typegpx == "R") {
          val listeFichiers = ("" +: new java.io.File("%s/contenu/gpx/randos/".
            format(Environment.simple.rootPath.getCanonicalPath))
            .listFiles.map(f => f.getName).filter(_.toLowerCase.endsWith(".gpx")).toSeq).diff(repoGpx.listGpxFiles)
            .map((0L, _, 0L))
          val listeMateriels = Seq[Materiel]()
          val listeMaterielsTrek = Seq[Materiel]()
          (listeFichiers, listeMateriels, listeMaterielsTrek)
        } else {
          val listeFichiers = repoGpx.listCandidatTrek(gpx.id).sortWith(_._2 < _._2)
          val listeMaterielsTrek = repoTrekMateriels.list(gpx.id).map(id => repoMateriels.get(id).get)
          val listeMateriels = repoMateriels.listAll.filter(m => !m.reforme || listeMaterielsTrek.contains(m))
          (listeFichiers, listeMateriels, listeMaterielsTrek)
        }
        val gpxForm = GpxForm(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description,
          gpx.listeMatos, gpx.nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale,
          gpx.descenteTotale, gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart, gpx.arrivee,
          gpx.coordonneesPix, gpx.typegpx,
          tableaux._1.filter(_._3 == gpx.id).map(f => f._1.toString + "," + f._2),
          tableaux._3.map(m => m.id.toString + "," + m.nom))
        Ok(views.html.gpx.form(this.gpxForm.fill(gpxForm),
          Option(id),
          listeCandidats= tableaux._1,
          listeMateriels = tableaux._2))
      }.getOrElse(NotFound)
    }
  }

  def updateGpx(id: Long): Action[MultipartFormData[Files.TemporaryFile]] =
      Action(parse.multipartFormData) { implicit request: MessagesRequest[MultipartFormData[TemporaryFile]] =>
    if (request.session.get("admin").isEmpty) {
      Redirect(routes.GpxController.showGpx(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val typegpx = request.body.dataParts("typegpx").head
      traiteRetour(request).fold(
        formWithErrors => {
          val tableaux = if(typegpx == "R") {
            val listeFichiers = ("" +: new java.io.File("%s/contenu/gpx/randos/".
              format(Environment.simple.rootPath.getCanonicalPath))
              .listFiles.map(f => f.getName).filter(_.toLowerCase.endsWith(".gpx")).toSeq).diff(repoGpx.listGpxFiles)
              .map((0L, _, 0L))
            val listeMateriels = Seq[Materiel]()
            (listeFichiers, listeMateriels)
          } else {
            val listeFichiers = repoGpx.listCandidatTrek(formWithErrors.data("id").toLong).sortWith(_._2 < _._2)
            val listeMateriels = repoMateriels.listAll
            (listeFichiers, listeMateriels)
          }
          Ok(views.html.gpx.form(formWithErrors,
            Option(id),
            listeCandidats = tableaux._1,
            listeMateriels = tableaux._2)).flashing(Flash(formWithErrors.data) +
            ("error" -> "Erreur de saisie"))
        },
        gpx => {
          if(typegpx == "R") {
            if (repoGpx.get(id).get.nomFichier == gpx.nomFichier) {
              repoGpx.update(Gpx(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos,
                gpx.nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale, gpx.descenteTotale,
                gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart,
                gpx.arrivee, gpx.coordonneesPix, gpx.typegpx))
            }
            else {
              repoGpx.update(Gpx.creer("R", gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description,
                gpx.listeMatos, gpx.nomFichier))
            }
          } else {
            val listeFichiers = for(f <- gpx.listeFichiers) yield {
              val sub = f.replaceAll("[\\(\\)]","").split(",")
              (sub(0),sub(1))
            }
            val listeFic = listeFichiers.map(_._2)
            val listeAExclure = repoGpx.listCandidatTrek(id).filter(_._3 == id).map(_._2)
            if(listeFic.size == listeAExclure.size && listeFic.diff(listeAExclure).isEmpty) {
              repoGpx.update(Gpx(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos,
                gpx.nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale, gpx.descenteTotale,
                gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart,
                gpx.arrivee, gpx.coordonneesPix, gpx.typegpx))
            } else {
              Gpx.fusion(listeFichiers.map(_._2), id.toString + ".gpx")
              repoGpx.update(Gpx.creer("T", gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre,
                gpx.description, gpx.listeMatos, id.toString + ".gpx"))
              repoGpx.rattacheGpx(id, listeFichiers.map(_._1.toLong))
            }
            val listeMaterielsARattacher = for(f <- gpx.listeMaterielsTreks) yield {
              val sub = f.replaceAll("[\\(\\)]","").split(",")
              sub(0).toLong
            }
            repoTrekMateriels.maj(gpx.id, listeMaterielsARattacher)
          }
          Redirect(routes.GpxController.showGpx(gpx.id))
            .flashing("success" -> ("Modification réussie du tracé " + id.toString))
        })}}
}
