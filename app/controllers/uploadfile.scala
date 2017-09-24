package controllers

import java.io.File

import play.api.libs.Files
import play.api.mvc.{MessagesRequest, MultipartFormData}

import scala.reflect.io.Path
import play.Environment

import sys.process._

trait uploadfile {

  def copyFile(fichierServeur: File, fichier: MultipartFormData.FilePart[Files.TemporaryFile]): String = {
    fichier.ref.moveTo(fichierServeur)
    fichierServeur.setReadable(true, false)
    fichierServeur.setWritable(true, false)
    if(scala.reflect.io.File(Path("%s/public".format(Environment.simple().rootPath().getCanonicalPath))).exists) {
      val origine = fichierServeur.getAbsolutePath
      val destination = origine.replaceAll("/contenu/", "/public/contenu/")
      val commande  = "cp %s %s".format(origine, destination)
      commande !
    }
    fichierServeur.getName
  }

  def uploadfile(request: MessagesRequest[MultipartFormData[Files.TemporaryFile]]): String = {
    val fichier = request.body.files.head
    val racine = fichier.key match {
      case "nouvellePhoto" => "images/matos"
      case "nouveauGpx" => "gpx/randos"
      case _ => ""
    }
    val fichierServeur = new java.io.File("%s/contenu/%s/%s".
      format(Environment.simple.rootPath.getCanonicalPath, racine, fichier.filename.replaceAll(" ","_")))
    copyFile(fichierServeur, fichier)
  }}
