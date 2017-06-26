package nextds.entity

import fastparse.core.ParseError
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.util.Try

/**
  * Created by pascal.mengelt on 24.06.2017.
  */
class FilterCondTest
  extends FlatSpec
    with Matchers
    with BeforeAndAfter {

  import FilterCond._

  "The FilterCond constructor" should "create a simple FilterElem correctly" in {
    assert(FilterCond("DE0zX").get === FilterElem("DE0zX"))
  }
  it should "should be possible to have AND/OR in the tags" in {
    assert(FilterCond("DEAND AND   OREN").get === FilterCalc(FilterElem("DEAND"), FilterElem("OREN"), AND))
  }
  it should "create a simple FilterCalc correctly" in {
    assert(FilterCond("DE AND   EN").get === FilterCalc(FilterElem("DE"), FilterElem("EN"), AND))
  }
  it should "create a simple OR FilterCalc correctly" in {
    assert(FilterCond("DE OR  EN").get === FilterCalc(FilterElem("DE"), FilterElem("EN"), OR))
  }
  it should "create (DE AND EN) FilterCalc correctly" in {
    assert(FilterCond("(DE AND EN)").get === FilterCalc(FilterElem("DE"), FilterElem("EN"), AND))
  }
  it should "create (DE AND EN) OR FR FilterCalc correctly" in {
    assert(FilterCond(
      """
        (DE AND EN)
          OR FR
      """).get === FilterCalc(FilterCalc(FilterElem("DE"), FilterElem("EN"), AND), FilterElem("FR"), OR))
  }
  it should "throw an exception if not correct syntax" in {
    val triedCond = FilterCond("(DE AND EN")
    assert(triedCond.failed.get match {
      case ParseError(failure) => true
      case other => false
    })
  }
  it should "possible to have spaces in the tag" in {
    val triedCond =
      assert(FilterCond("DE EN").get == FilterElem("DE EN"))
  }

  private val deFilter = FilterCond("DE").get
  private val deEnOrFilter = FilterCond("DE OR EN").get
  private val deEnAndfilter = FilterCond("DE AND EN").get
  private val frEnOrFilter = FilterCond("FR OR EN").get
  private val itFrOrFilter = FilterCond("IT OR FR").get

  "The elements FilterCond DE" should "adhere to the containers DE" in {
    assert(deFilter.adheresFilter(deFilter))
  }
  it should "NOT adhere to the containers EN" in {
    assert(!FilterCond("EN").get.adheresFilter(deFilter))
  }
  it should "adhere to the containers DE OR EN" in {
    assert(deEnOrFilter.adheresFilter(deFilter))
  }
  it should "NOT adhere to the containers FR OR EN" in {
    assert(!frEnOrFilter.adheresFilter(deFilter))
  }
  it should "NOT adhere to the containers DE AND EN" in {
    assert(!deEnAndfilter.adheresFilter(deFilter))
  }

  "The elements FilterCond DE OR EN" should "adhere to the containers FR OR EN" in {
    assert(frEnOrFilter.adheresFilter(deEnOrFilter))
  }
  it should "adhere to the containers DE" in {
    assert(deFilter.adheresFilter(deEnOrFilter))
  }
  it should "NOT adhere to the containers IT" in {
    assert(!FilterCond("IT").get.adheresFilter(deEnOrFilter))
  }
  it should "NOT adhere to the containers IT OR FR" in {
    assert(!itFrOrFilter.adheresFilter(deEnOrFilter))
  }
}
