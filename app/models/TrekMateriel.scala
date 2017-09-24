package models

import javax.inject.{Inject, Singleton}
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

case class TrekMateriel(id: Long, idTrek: Long, idMateriel: Long) {
}

@Singleton
class TrekMaterielRep @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {

  val dbConfig = dbConfigProvider.get[JdbcProfile]

  // These imports are important, the first one brings db into scope, which will let you do the actual db operations.
  // The second one brings the Slick DSL into scope, which lets you define the table and other queries.
  import dbConfig._
  import profile.api._

  private class TrekMaterielTable(tag: Tag) extends Table[TrekMateriel](tag, "trekmateriel") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def idTrek = column[Long]("idtrek")
    def idMateriel = column[Long]("idmateriel")


    def * = (id, idTrek, idMateriel) <> ((TrekMateriel.apply _).tupled, TrekMateriel.unapply)
  }

  /**
    * The starting point for all queries on the people table.
    */
  private val trekMateriels = TableQuery[TrekMaterielTable]

  def maj(idTrek: Long, listeMatosId: Seq[Long]): Seq[Long] = {
    val listTrekMateriel = for(idMateriel <- listeMatosId) yield TrekMateriel(0L, idTrek, idMateriel)
    detache(idTrek)
    val fut = db.run((trekMateriels returning trekMateriels.map(_.id)) ++= listTrekMateriel)
    Await.result(fut, 2.seconds)
  }

  def exist(idTrek: Long): Boolean = {
    val fut = db.run(trekMateriels.filter(_.idTrek === idTrek).result.headOption)
    Await.result(fut, 2.seconds).isDefined
  }

  def list(idTrek: Long): Seq[Long] = {
    val fut = db.run(trekMateriels.filter(_.idTrek === idTrek).map(_.idMateriel).result)
    Await.result(fut, 2.seconds)
  }

  def detache(idTrek: Long): Int = {
    val fut = db.run(trekMateriels.filter(_.idTrek === idTrek).delete)
    Await.result(fut, 2.seconds)
  }}
