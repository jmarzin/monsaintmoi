package models

import org.joda.time.DateTime
import play.Environment
import javax.inject.{Inject, Singleton}
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.reflect.io.Path
import scala.xml.XML

case class Gpx(id: Long, idTrek: Long, titre: String, sousTitre: String, description: String, listeMatos: String, nomFichier: String, altitudeMinimum: Int, altitudeMaximum: Int, ascensionTotale: Int, descenteTotale: Int,
               heureDebut: String, heureFin: String, distanceTotale: Int, depart: String,
               arrivee: String, coordonneesPix: String, typegpx: String) {
}

object Gpx {

  val precision = 1000D

  private def distance(pointA: (BigDecimal, BigDecimal), pointB: (BigDecimal, BigDecimal)): Double = {
    val a = 6378137D
    val b = 6356752.314245D
    val f = 1 / 298.257223563
    val L = Math.toRadians((pointB._2 - pointA._2).toDouble)
    val U1 = Math.atan((1 - f) * Math.tan(Math.toRadians(pointA._1.toDouble)))
    val U2 = Math.atan((1 - f) * Math.tan(Math.toRadians(pointB._1.toDouble)))
    val sinU1 = Math.sin(U1)
    val cosU1 = Math.cos(U1)
    val sinU2 = Math.sin(U2)
    val cosU2 = Math.cos(U2)
    var cosSqAlpha = .0
    var sinSigma = .0
    var cos2SigmaM = .0
    var cosSigma = .0
    var sigma = .0
    var lambda = L
    var lambdaP = .0
    var iterLimit = 100
    do {
      val sinLambda = Math.sin(lambda)
      val cosLambda = Math.cos(lambda)
      sinSigma = Math.sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda) +
        (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda) * (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda))
      if (sinSigma == 0) return 0
      cosSigma = sinU1 * sinU2 + cosU1 * cosU2 * cosLambda
      sigma = Math.atan2(sinSigma, cosSigma)
      val sinAlpha = cosU1 * cosU2 * sinLambda / sinSigma
      cosSqAlpha = 1 - sinAlpha * sinAlpha
      cos2SigmaM = cosSigma - 2 * sinU1 * sinU2 / cosSqAlpha
      val C = f / 16 * cosSqAlpha * (4 + f * (4 - 3 * cosSqAlpha))
      lambdaP = lambda
      lambda = L + (1 - C) * f * sinAlpha *
        (sigma + C * sinSigma * (cos2SigmaM + C * cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM)))
    } while ( {
      Math.abs(lambda - lambdaP) > 1e-12 && {
        iterLimit -= 1; iterLimit
      } > 0
    })
    if (iterLimit == 0) return 0
    val uSq = cosSqAlpha * (a * a - b * b) / (b * b)
    val A = 1 + uSq / 16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)))
    val B = uSq / 1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)))
    val deltaSigma = B * sinSigma *
      (cos2SigmaM + B / 4 * (cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM) - B / 6 * cos2SigmaM *
        (-3 + 4 * sinSigma * sinSigma) * (-3 + 4 * cos2SigmaM * cos2SigmaM)))
    b * A * (sigma - deltaSigma)
  }

  def creer(typegxp: String, id: Long, idTrek: Long, titre: String,
            sousTitre: String, description: String, listeMatos: String, filename: String): Gpx = {

    var altitudes: Seq[Seq[Double]] = Seq()
    var altitudeMinimum: Int = Int.MaxValue
    var altitudeMaximum: Int = Int.MinValue
    var ascensionTotale: Int = 0
    var descenteTotale: Int = 0
    var distanceTotale: Int = 0
    var distancesCumulees: Seq[Seq[Double]] = Seq()

    val racine = (typegxp match {
      case "R" => "%s/contenu/gpx/randos/"
      case "T" => "%s/contenu/gpx/treks/"
    }).format(Environment.simple.rootPath.getAbsolutePath)

    val xml = XML.loadFile(racine + filename)
    var ini = 0D
    // les dénivelés et distances sont calculés piste par piste et ne prennent pas en compte leur disjonction éventuelle
    for(trk <- xml \\ "trk") {
      val alt = (trk \\ "ele").map(_.text.toDouble)
      altitudes +:= alt
      altitudeMinimum = altitudeMinimum min alt.min.toInt
      altitudeMaximum = altitudeMaximum max alt.max.toInt
      val diffAltitudes = (alt.tail zip alt).map(f => f._1 - f._2)
      ascensionTotale += diffAltitudes.filter(_ > 0).sum.toInt
      descenteTotale += diffAltitudes.filter(_ < 0).sum.abs.toInt
      val trkpt = (trk \\ "trkpt").map(t => (BigDecimal(t \ "@lat" text) , BigDecimal(t \ "@lon" text)))
      val distances = (trkpt.tail zip trkpt).map(p => distance(p._1, p._2))
      distanceTotale += distances.sum.toInt
      val dist = ini +: (for (d <- distances) yield {
        ini = ini + d
        ini
      })
      distancesCumulees +:= dist
    }
    val reductionAlt = (altitudeMaximum - altitudeMinimum)/ Gpx.precision
    val altitudesPix = for (t <- altitudes) yield t.map(a => (1000 - (a - altitudeMinimum) / reductionAlt).toInt)
    val reductionDist = distanceTotale / (2 * Gpx.precision)
    val distancesCumuleesPix = for (t <- distancesCumulees) yield t.map(d => (d / reductionDist) toInt)
    val heures = (xml \\ "trk" \\ "time").map(h => new DateTime(h.text).getMillis)
    val heureDebut = if(heures.isEmpty) "" else new DateTime(heures.min).toString()
    val heureFin = if(heures.isEmpty) "" else new DateTime(heures.max).toString()
    val trkpt = (xml \\ "trkpt").map(t => (BigDecimal(t \ "@lat" text) , BigDecimal(t \ "@lon" text)))
    val arrivee = trkpt.last
    val depart = trkpt.head
    val coordonneesPix = for(i <- altitudesPix.indices) yield (distancesCumuleesPix(i) zip altitudesPix(i)).distinct
    new Gpx(id, idTrek, titre, sousTitre, description, listeMatos, filename, altitudeMinimum, altitudeMaximum,
      ascensionTotale, descenteTotale, heureDebut, heureFin,
      distanceTotale, "%s,%s".format(depart._1, depart._2), "%s,%s".format(arrivee._1, arrivee._2),
      { var html = ""
        for(trace <- coordonneesPix) {
          html += "<polyline points=\""
          for(c <- trace) {
            html += c._1 + "," + c._2 + " "
          }
          html += "\" style=\"fill: none;stroke: #f00;stroke-width: 5\"></polyline>\n"
        }
        html
      },
      typegxp)
  }

  def fusion(listeFichiersGpx: Seq[String], fichierGpxResultat: String) : Unit = {
    var resultat = ""
    for (fichier <- listeFichiersGpx) {
      val xml = XML.loadFile("%s/contenu/gpx/randos/%s".
        format(Environment.simple.rootPath.getAbsolutePath, fichier))
      if (resultat == "") {
        resultat = xml.toString()
      } else {
        val pattern = "(?s).*(<trk>.*?</trk>).*".r
        val pattern(trace) = xml.toString()
        resultat = resultat.replaceAll("(?s)</gpx>",
          "  %s\n\n</gpx>".format(trace))
      }}

    val xml = XML.loadString(resultat)
    val latitudes = (xml \\ "@lat").map(l => BigDecimal(l.text))
    val longitudes = (xml \\ "@lon").map(l => BigDecimal(l.text))
    resultat = resultat.replaceAll("(?s)<bounds .*?/>",
      "<bounds maxlat=\"%s\" maxlong=\"%s\" minlat=\"%s\" minlon=\"%s\"/>".
        format(latitudes.max, longitudes.max, latitudes.min, longitudes.min))
    XML.save("%s/contenu/gpx/treks/%s".
      format(Environment.simple.rootPath.getCanonicalPath, fichierGpxResultat), XML.loadString(resultat))
    if (scala.reflect.io.File(Path("%s/public".
      format(Environment.simple().rootPath().getCanonicalPath))).exists) {
      XML.save("%s/public/contenu/gpx/treks/%s".
        format(Environment.simple.rootPath.getCanonicalPath, fichierGpxResultat),
        XML.loadString(resultat))
    }}}

@Singleton
class GpxRep @Inject() (dbConfigProvider: DatabaseConfigProvider,
                        repoTrekMateriels: TrekMaterielRep)(implicit ec: ExecutionContext) {

  val dbConfig = dbConfigProvider.get[JdbcProfile]

  // These imports are important, the first one brings db into scope, which will let you do the actual db operations.
  // The second one brings the Slick DSL into scope, which lets you define the table and other queries.
  import dbConfig._
  import profile.api._

  private class GpxTable(tag: Tag) extends Table[Gpx](tag, "gpx") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def idTrek = column[Long]("idtrek")
    def titre = column[String]("titre")
    def sousTitre = column[String]("soustitre")
    def description = column[String]("description")
    def listeMatos = column[String]("listematos")
    def nomFichier = column[String]("nomfichier")
    def altitudeMinimum = column[Int]("altitudeminimum")
    def altitudeMaximum = column[Int]("altitudemaximum")
    def ascensionTotale = column[Int]("ascensiontotale")
    def descenteTotale = column[Int]("descentetotale")
    def heureDebut = column[String]("heuredebut")
    def heureFin = column[String]("heurefin")
    def distanceTotale = column[Int]("distancetotale")
    def depart = column[String]("depart")
    def arrivee = column[String]("arrivee")
    def coordonneesPix = column[String]("coordonneespix")
    def typegpx = column[String]("typegpx")


    def * = (id, idTrek, titre, sousTitre, description, listeMatos, nomFichier, altitudeMinimum, altitudeMaximum,
      ascensionTotale, descenteTotale, heureDebut, heureFin, distanceTotale, depart, arrivee, coordonneesPix, typegpx) <> ((Gpx.apply _).tupled, Gpx.unapply)
  }

  /**
    * The starting point for all queries on the gpx table.
    */
  private val gpxs = TableQuery[GpxTable]

  private def exec[T](action: DBIO[T]): T =
    Await.result(db.run(action), 2 seconds)

  def add(gpx: Gpx): Long = {
    exec((gpxs returning gpxs.map(_.id)) += gpx)
  }

  def annuleTrek(id: Long): Int = {
    val query = gpxs.filter(_.idTrek === id).map(g => g.idTrek)
    exec(query.update(0L))
  }

  def removeById(id: Long): Int = {
    val fut = db.run(gpxs.filter(_.id === id).delete)
    Await.result(fut, 2.seconds)
    repoTrekMateriels.detache(id)
    annuleTrek(id)
  }

  def get(id: Long): Option[Gpx] = {
    val fut = db.run(gpxs.filter(_.id === id).result.headOption)
    Await.result(fut, 2.seconds)
  }

  def update(gpx: Gpx) : Int = {
    val query = gpxs.
      filter(_.id === gpx.id).
      map(g => (g.idTrek, g.titre, g.sousTitre, g.description, g.nomFichier, g.altitudeMinimum, g.altitudeMaximum,
        g.ascensionTotale, g.descenteTotale, g.heureDebut, g.heureFin, g.distanceTotale, g.depart, g.arrivee, g.coordonneesPix,
        g.listeMatos))
    val fut = db.run(query.update(gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.nomFichier,
      gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale, gpx.descenteTotale, gpx.heureDebut, gpx.heureFin,
      gpx.distanceTotale, gpx.depart, gpx.arrivee, gpx.coordonneesPix, gpx.listeMatos))
    Await.result(fut, 2.seconds)
  }

  def listAll(typeGpx: String): Seq[Gpx] = {
    val fut = db.run(gpxs.filter(_.typegpx === typeGpx).result)
    Await.result(fut, 2.seconds)
  }

  def listByTrekId(id: Long): Seq[Gpx] = {
    val fut= db.run(gpxs.filter(_.idTrek === id).result)
    Await.result(fut, 2.seconds)
  }

  def listCandidatTrek(id: Long): Seq[(Long, String, Long)] = {
    val fut = db.run(gpxs.filter(g => g.typegpx === "R" && (g.idTrek === 0L || g.idTrek === id)).
      map(g => (g.id, g.nomFichier, g.idTrek)).result)
    Await.result(fut, 2.seconds)
  }

  def lastValueIndex: Long = {
    val fut = db.run(sql"""select last_value from gpx_id_seq""".as[Long])
    Await.result(fut, 2.seconds).head
  }

  def listGpxFiles: Seq[String] = {
    val fut = db.run(gpxs.filter(_.typegpx === "R").map(f => f.nomFichier).result)
    Await.result(fut, 2.seconds)
  }

  def rattacheGpx(idTrek: Long, listeFichiers: Seq[Long]): Unit = {
    val query = gpxs.
      filter(_.idTrek === idTrek).
      map(g => g.idTrek)
    val fut = db.run(query.update(0L))
    Await.result(fut, 2.seconds)
    for(f <- listeFichiers) {
      val query = gpxs.
        filter(_.id === f).
        map(g => g.idTrek)
      val fut = db.run(query.update(idTrek))
      Await.result(fut, 2.seconds)
    }}}
