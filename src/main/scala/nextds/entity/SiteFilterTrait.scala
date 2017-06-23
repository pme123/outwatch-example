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

case class SiteFilter(siteIdent: String, ident: String, title: String, descr: String = "No description available")

object SiteFilter {
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

trait FilterTag {

  def siteIdent: String

  def tag: String

  def descr: String

  def addChildren(childTags: String*): FilterTag


  def filterTags(filterTag: String): Seq[FilterTag] =
    if (filterTag == tag)
      Seq(this)
    else
      filterRest(filterTag)

  protected def filterRest(filterTag: String): Seq[FilterTag]

}

case class TagGroup(siteIdent: String
                    , tag: String
                    , children: Seq[FilterTag] = Nil
                    , descr: String = "No description available") extends FilterTag {

  def addChildren(childTags: String*): FilterTag =
    copy(children = children ++ childTags.map(Tag(siteIdent, _)))

  protected def filterRest(filterTag: String): Seq[FilterTag] =
    children.flatMap(_.filterTags(filterTag))

}

case class Tag(siteIdent: String
               , tag: String
               , descr: String = "No description available") extends FilterTag {

  def addChildren(childTags: String*): FilterTag =
    TagGroup(siteIdent, tag, Nil, descr).addChildren(childTags: _*)

  protected def filterRest(filterTag: String): Seq[FilterTag] = Nil


}

case class FilterTagConf(siteIdent: String
                         , ident: String
                         , condition: String
                         , filterTags: Seq[FilterTag] = Nil
                         , descr: String = "No description available") {

}

object FilterTagConf {
  def apply(siteIdent: String, condition: String, filterTags: Seq[FilterTag]): FilterTagConf =
    FilterTagConf(siteIdent, Site.nextIdent(siteIdent), condition, filterTags)

}


case class FilterTags(filterTags: Seq[FilterTag]) {
  def filterTags(tag: String): Seq[FilterTag] =
    filterTags.flatMap(ft => ft.filterTags(tag: String))

  def filterTag(tag: String): FilterTag =
    filterTags(tag).head

  def filterTagOpt(tag: String): Option[FilterTag] =
    filterTags(tag).headOption

}
