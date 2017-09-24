package models

import javax.inject.{Inject, Singleton}
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

case class Materiel(id: Long, nom: String, description: String, photo: String, poids: Int, reforme: Boolean) {

  def descriptionCourte: String = {
    val desc = this.description.replaceAll("<.*?>","")
      .replaceAll("&eacute;","é")
      .replaceAll("&agrave;","à")
      .replaceAll("&acirc;","â")
      .replaceAll("&egrave;","è")
      .replaceAll("&ecirc;","ê")
      .replaceAll("&ocirc;","ô")
      .replaceAll("&ugrave;","ù")
      .replaceAll("&ucirc;","û")
      .replaceAll("&rsquo;", "'")
      .replaceAll("&nbsp;", " ")
      .replaceAll("&ccedil;", "ç")
    val coupure = desc.indexOf(" ", 120)
    if(coupure == -1) {
      return desc
    }
    desc.substring(0, coupure) + " ..."
  }}

@Singleton
class MaterielRep @Inject() (dbConfigProvider: DatabaseConfigProvider,
                             repoTrekMateriels: TrekMaterielRep)(implicit ec: ExecutionContext) {

  val dbConfig = dbConfigProvider.get[JdbcProfile]

  // These imports are important, the first one brings db into scope, which will let you do the actual db operations.
  // The second one brings the Slick DSL into scope, which lets you define the table and other queries.
  import dbConfig._
  import profile.api._

  private class MaterielTable(tag: Tag) extends Table[Materiel](tag, "materiel") {

    def id = column[Long]("id", O.PrimaryKey,O.AutoInc)
    def nom = column[String]("nom")
    def description = column[String]("description")
    def photo = column[String]("photo")
    def poids = column[Int]("poids")
    def reforme = column[Boolean]("reforme")

    def * = (id, nom, description, photo, poids, reforme) <> ((Materiel.apply _).tupled, Materiel.unapply)
  }

  /**
    * The starting point for all queries on the material table.
    */
  private val materiels = TableQuery[MaterielTable]

  def add(materiel: Materiel): Long = {
    val fut = db.run((materiels returning materiels.map(_.id)) += materiel)
    Await.result(fut, 2.seconds)
  }

  def removeById(id: Long): Int = {
    val fut = db.run(materiels.filter(_.id === id).delete)
    Await.result(fut, 2.seconds)
  }

  def get(id: Long): Option[Materiel] = {
    val fut = db.run(materiels.filter(_.id === id).result.headOption)
    Await.result(fut, 2.seconds)
  }

  def update(materiel:Materiel) : Int = {
    val query = materiels.
      filter(_.id === materiel.id).
      map(mat => (mat.nom, mat.description, mat.photo, mat.poids, mat.reforme))
    val fut = db.run(query.update(materiel.nom, materiel.description, materiel.photo, materiel.poids, materiel.reforme))
    Await.result(fut, 2.seconds)
  }

  def listAll: Seq[Materiel] = {
    val fut = db.run(materiels.result)
    Await.result(fut, 2.seconds)
  }

  def listTrekMateriel(idTrek: Long): Seq[Materiel] = {
    val listeMateriels = repoTrekMateriels.list(idTrek)
    for(idMat <- listeMateriels) yield {
      val fut = db.run(materiels.filter(_.id === idMat).result.head)
      Await.result(fut, 2.seconds)
    }}}
