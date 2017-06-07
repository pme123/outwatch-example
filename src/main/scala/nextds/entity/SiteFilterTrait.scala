package nextds.entity

/**
  * Created by pascal.mengelt on 07.06.2017.
  */
trait SiteFilterTrait extends SiteEntityTrait {
  def siteFilter: SiteFilter

  lazy val levelType: LevelType = FILTER
  lazy val siteIdent: String = siteFilter.siteId
  lazy val title: String = siteFilter.title
  lazy val descr: String = siteFilter.descr

  lazy val maybeTitle: Option[String] = Some(title)
  lazy val maybeDescr: Option[String] = Some(descr)

}

case class SiteFilter(siteId: String, title: String, descr: String = "No description available")

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
