package models

import org.scalatest.FunSuite

class MaDateTest extends FunSuite {

  test("Ecart entre 1/1/2017 et 2/1/2017 est 1 jour") {
    assert(MaDate(2017,1,1).ecartA(MaDate(2017,1,2)) == "1 jour")
  }
  test("Ecart entre 1/1/2017 et 3/1/2017 est 2 jours") {
    assert(MaDate(2017,1,1).ecartA(MaDate(2017,1,3)) == "2 jours")
  }
  test("Ecart entre 1/1/2017 et 1/2/2017 est 1 mois") {
    assert(MaDate(2017,1,1).ecartA(MaDate(2017,2,1)) == "1 mois")
  }
  test("Ecart entre 1/1/2017 et 3/2/2017 est 1 mois 2 jours") {
    assert(MaDate(2017,1,1).ecartA(MaDate(2017,2,3)) == "1 mois 2 jours")
  }
  test("Ecart entre 28/1/2017 et 2/2/2017 est 5 jours") {
    assert(MaDate(2017,1,28).ecartA(MaDate(2017,2,2)) == "5 jours")
  }
  test("Ecart entre 28/12/2017 et 2/1/2018 est 5 jours") {
    assert(MaDate(2017,12,28).ecartA(MaDate(2018,1,2)) == "5 jours")
  }
  test("Ecart entre 28/11/2017 et 2/2/2018 est 2 mois 5 jours") {
    assert(MaDate(2017,11,28).ecartA(MaDate(2018,2,2)) == "2 mois 5 jours")
  }
  test("Ecart entre 28/2/2017 et 3/3/2017 est 3 jours") {
    assert(MaDate(2017,2,28).ecartA(MaDate(2017,3,3)) == "3 jours")
  }
  test("Ecart entre 28/4/2017 et 3/5/2017 est 5 jours") {
    assert(MaDate(2017,4,28).ecartA(MaDate(2017,5,3)) == "5 jours")
  }
}
