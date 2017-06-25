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

}
