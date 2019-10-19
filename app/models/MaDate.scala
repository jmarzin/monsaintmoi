package models

case class MaDate(var annee: Int, var mois: Int, var jour: Int) {

  private def ecartTexte(valeur: Int, mot: String): String= {
    if (valeur <= 0) return ""
    if (valeur == 1 || mot.endsWith("s")) return valeur + " " + mot
    valeur + " " + mot + "s"
  }

  def ecartA(date: MaDate): String= {
    if (date.jour < this.jour) {
      date.mois match {
        case 2 | 4 | 6 | 8 | 10 | 12 => date.jour += 31
        case 3 => date.jour += 28
        case _ => date.jour += 30
      }
      date.mois -= 1
    }
    if (date.mois < this.mois) {
      date.mois += 12
      date.annee -= 1
      if(date.mois % 2 == 0) date.jour+=1
    }
    if (date.annee < this.annee) {
      "Je suis déjà parti !"
    } else {
      (ecartTexte(date.annee - this.annee, "année") + " "
        + ecartTexte(date.mois - this.mois, "mois") + " "
        + ecartTexte(date.jour - this.jour, "jour")).replaceAll("  ", " ").trim
    }}}
