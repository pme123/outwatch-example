package nextds.entity

/**
  * Created by pascal.mengelt on 07.06.2017.
  */
trait SiteFilterTrait extends SiteEntityTrait {
  lazy val levelType: LevelType = FILTER

}

trait FilterTag {

  def siteIdent: String

  def tag: String

  def descr: String

  def addChildren(childTags: String*): FilterTag

  def parent: Option[FilterTag]

  def path: String = parent.map(_.path).getOrElse("") + s"> $tag "

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
                    , descr: String = "No description available"
                    , parent: Option[TagGroup] = None) extends FilterTag {

  def addChildren(childTags: String*): FilterTag =
    copy(children = children ++ childTags.map(Tag(siteIdent, _, parent=Some(this))))

  protected def filterRest(filterTag: String): Seq[FilterTag] =
    children.flatMap(_.filterTags(filterTag))

}

case class Tag(siteIdent: String
               , tag: String
               , descr: String = "No description available"
               , parent: Option[TagGroup] = None) extends FilterTag {

  def addChildren(childTags: String*): FilterTag =
    TagGroup(siteIdent, tag, Nil, descr).addChildren(childTags: _*)

  protected def filterRest(filterTag: String): Seq[FilterTag] = Nil


}

case class FilterTagConf(siteIdent: String
                         , ident: String
                         , condition: String
                         , filterTags: Seq[FilterTag] = Nil
                         , descr: String = "No description available") extends SiteFilterTrait {

  def siteType: SiteType = FILTER_TAG

  def title: String = condition

  def maybeTitle: Option[String] = Some(title)

  def maybeDescr: Option[String] = Some(descr)
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
