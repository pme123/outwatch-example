package nextds.entity

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

/**
  * Created by pascal.mengelt on 26.06.2017.
  */
class SiteConfInfoTraitTest
  extends FlatSpec
    with Matchers
    with BeforeAndAfter {

  import TestResources._

  "The filterLinks of a PlayerConf" should "should contain itself" in {
    assert(playerConf.filterLinks(Set()) === Set(playerConf))
  }
  it should "contain all SiteEntities that are in the siteEntities Set" in {
    assert(playerConf.filterLinks(Set(layoutTempl)) === Set(playerConf, layoutTempl))
  }
  it should "contain all SiteConfs with the same FilterTagConf" in {
    assert(playerConf.filterLinks(Set(playerConf, layoutConf, regionConf, playistConf, mediumConf)) === Set(playerConf, layoutConf, regionConf, playistConf, mediumConf))
  }
  it should "contain all SiteConfs if the selected has no Filter" in {
    assert(playerConfNoFilter.filterLinks(Set(playerConfNoFilter, layoutConf, regionConf, playistConf, mediumConf)) === Set(playerConfNoFilter, layoutConf, regionConf, playistConf, mediumConf))
  }
  it should "contain all SiteConfs if one has no Filter" in {
    assert(playerConf.filterLinks(Set(playerConf, layoutConf, regionConf, playistConf, mediumConfNoFilter)) === Set(playerConf, layoutConf, regionConf, playistConf, mediumConfNoFilter))
  }
  it should "contain NOT all SiteConfs if one has a Filter that does not adhere" in {
    assert(playerConf.filterLinks(Set(playerConf, layoutConf, regionConfOtherFilter, playistConf, mediumConf)) === Set(playerConf, layoutConf))
  }
  "The filterLinks of a MediumConf" should "contain all SiteConfs with the same FilterTagConf" in {
    assert(mediumConf.filterLinks(Set(playerConf, layoutConf, regionConf, playistConf, mediumConf)) === Set(playerConf, layoutConf, regionConf, playistConf, mediumConf))
  }
  it should "contain all SiteConfs if the selected has no Filter" in {
    assert(mediumConfNoFilter.filterLinks(Set(playerConf, layoutConf, regionConf, playistConf, mediumConfNoFilter)) === Set(playerConf, layoutConf, regionConf, playistConf, mediumConfNoFilter))
  }

  "The filterLinks of a Comp" should "should be the linked set" in {
    assert(playerComp.filterLinks(Set()) === Set())
  }
  "The filterLinks of a Templ" should "should be the linked set" in {
    assert(playerTempl.filterLinks(Set(layoutTempl)) === Set(layoutTempl))
  }

}

object TestResources {
  val siteIdent = "site"
  private val tagConf = FilterTagConf(siteIdent, "DE", FilterTags(Seq()))
  private val tagConf2 = FilterTagConf(siteIdent, "FR", FilterTags(Seq()))


  val layoutTempl: LayoutTempl = LayoutTempl.singleLayout(SiteEntityInfo(siteIdent, "layout 1"), ScreenRegion())
  val layoutComp = LayoutComp(SiteComp(siteIdent, layoutTempl))
  val layoutConf = LayoutConf(layoutComp, tagConf)
  val regionConf = RegionConf(layoutComp, tagConf)
  val regionConfOtherFilter = RegionConf(layoutComp, tagConf2)

  val playistTempl = PlaylistTempl(SiteEntityInfo(siteIdent, "playist 1"))
  val playistComp = PlaylistComp(SiteComp(siteIdent, playistTempl))
  val playistConf = PlaylistConf(playistComp, tagConf)

  val mediumTempl = MediumTempl(SiteEntityInfo(siteIdent, "medium 1"))
  val mediumComp = MediumComp(SiteComp(siteIdent, mediumTempl))
  val mediumConf = MediumConf(mediumComp, tagConf)
  val mediumConfNoFilter = MediumConf(siteIdent, mediumComp)

  val playerTempl = PlayerTempl(SiteEntityInfo(siteIdent, "player 1"))
  val playerComp = PlayerComp(SiteComp(siteIdent, playerTempl))
  val playerConf = PlayerConf(playerComp, tagConf)
  val playerConfNoFilter = PlayerConf(playerComp)

}
