package nextds.entity

import fastparse.WhitespaceApi

import scala.util.Try


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
    copy(children = children ++ childTags.map(Tag(siteIdent, _, parent = Some(this))))

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

sealed trait FilterCond {

}


object FilterCond {

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(Seq(' ', '\n', '\t', '\r')).rep)
  }

  import fastparse.noApi._
  import White._

  def eval(tree: (FilterCond, Seq[(String, FilterCond)])): FilterCond = {
    val (base, ops) = tree
    ops.foldLeft(base) { case (left, (op, right)) =>
      FilterCalc(left, right, op match {
        case "&" => AND
        case "|" => OR
      })
    }
  }

  val chars: P[FilterCond] = P(CharIn('0' to '9', 'a' to 'z', 'A' to 'Z', Seq('-', '_', '$', '?', '!')).rep(1).!).map(FilterElem)
  val parens: P[FilterCond] = P("(" ~/ orOp ~ ")")
  val factor: P[FilterCond] = P(chars | parens)

  val andOp: P[FilterCond] = P(factor ~ (CharIn("&").! ~/ factor).rep).map(eval)
  val orOp: P[FilterCond] = P(andOp ~ (CharIn("|").! ~/ andOp).rep).map(eval)
  val expr: P[FilterCond] = P(" ".rep ~ orOp ~ " ".rep ~ End)

  def apply(cond: String): Try[FilterCond] = {
    val str = cond
      .replace(AND.repr, "&")
      .replace(OR.repr, "|")
      .trim
    Try(expr.parse(str).get.value)
  }

  case class FilterCalc(left: FilterCond, right: FilterCond, operator: FilterOperator) extends FilterCond

  case class FilterElem(tag: String) extends FilterCond

  sealed trait FilterOperator

  case object AND extends FilterOperator {
    val repr = " AND "
  }

  case object OR extends FilterOperator {
    val repr = " OR "
  }

}

case class FilterTags(filterTags: Seq[FilterTag]) {
  def filterTags(tag: String): Seq[FilterTag] =
    filterTags.flatMap(ft => ft.filterTags(tag: String))

  def filterTag(tag: String): FilterTag =
    filterTags(tag).head

  def filterTagOpt(tag: String): Option[FilterTag] =
    filterTags(tag).headOption

}
