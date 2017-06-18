package nextds.entity

import nextds.server.control.SiteFilterRepo.publicSiteIdent

/**
  * Created by pascal.mengelt on 07.06.2017.
  */
trait SiteFilterTrait extends SiteEntityTrait {
  def siteFilter: SiteFilter

  lazy val levelType: LevelType = FILTER
  lazy val siteIdent: String = siteFilter.siteIdent
  lazy val ident: String = siteFilter.ident
  lazy val title: String = siteFilter.title
  lazy val descr: String = siteFilter.descr

  lazy val maybeTitle: Option[String] = Some(title)
  lazy val maybeDescr: Option[String] = Some(descr)

}

case class SiteFilter(siteIdent: String, ident:String, title: String, descr: String = "No description available")
object SiteFilter{
  def apply(siteIdent: String, title: String): SiteFilter =
    new SiteFilter(siteIdent, Site.nextIdent(publicSiteIdent), title)
}
case class TagFilter(siteFilter: SiteFilter)
  extends SiteFilterTrait
    with SiteEntityTrait {
  val siteType = TAG_FILTER

}

case class TimeFilter(siteFilter: SiteFilter)
  extends SiteFilterTrait
    with SiteEntityTrait {
  val siteType = TIME_FILTER

}
