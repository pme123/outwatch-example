package nextds.entity

import fastparse.WhitespaceApi

import scala.util.Try


/**
  * Created by pascal.mengelt on 07.06.2017.
  */
trait SiteFilterTrait extends SiteEntityTrait {
  lazy val levelType: LevelType = FILTER

}

/**
  * Filtering Rules:
  * - from left to right: PLAYER > LAYOUT > REGION > PLAYLIST > MEDIUM
  * - these relations we call always: container > elements
  * - if no Filters are defined in a container: all elements are added
  * - if no Filters are defined in an element: it is added to every container
  * - a container describes what elements are added: e.g. DE OR (EN AND ORP)
  * - an element is added, when:
  *    - the element has no Filter
  *    - the element adheres to its container Filters, e.g. the REGION must adhere to the Filters of its LAYOUTs and its PLAYERs
  *    - the container has no Filter
  *
  */
sealed trait FilterTag {

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

  def findPossibleTags(tagStr: SiteIdent): Seq[Tag]

  def allTags(): Seq[String]


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

  def findPossibleTags(tagStr: String): Seq[Tag] = children.flatMap(_.findPossibleTags(tagStr))

  def allTags(): Seq[String] = children.flatMap(_.allTags())
}

object TagGroup {

}

case class Tag(siteIdent: String
               , tag: String
               , descr: String = "No description available"
               , parent: Option[TagGroup] = None) extends FilterTag {

  def addChildren(childTags: String*): FilterTag =
    TagGroup(siteIdent, tag, Nil, descr).addChildren(childTags: _*)

  protected def filterRest(filterTag: String): Seq[FilterTag] = Nil

  def findPossibleTags(tagStr: SiteIdent): Seq[Tag] =
    if (tag.contains(tagStr)) Seq(this) else Nil

  def allTags(): Seq[String] = Seq(tag)

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
  def filterTags: Seq[String]

  // tested for basic cases
  def adheresFilter(elemFilter: FilterCond): Boolean

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

  case class FilterCalc(left: FilterCond, right: FilterCond, operator: FilterOperator) extends FilterCond {
    def filterTags: Seq[String] = left.filterTags ++ right.filterTags

    def adheresFilter(elemFilter: FilterCond): Boolean = (this, elemFilter) match {
      case (FilterCalc(l,r,OR),FilterElem(_)) => l.adheresFilter(elemFilter) || r.adheresFilter(elemFilter)
      case (FilterCalc(l,r,AND),FilterElem(_)) => l.adheresFilter(elemFilter) && r.adheresFilter(elemFilter)
      case (FilterCalc(l,r,AND),FilterCalc(el,er,_)) =>
        (l.adheresFilter(el) || l.adheresFilter(er)) && (r.adheresFilter(el) || r.adheresFilter(er))
      case (FilterCalc(l,r,OR),FilterCalc(el,er,OR)) =>
        (l.adheresFilter(el) || l.adheresFilter(er)) || (r.adheresFilter(el) || r.adheresFilter(er))
      case (FilterCalc(l,r,OR),FilterCalc(el,er,AND)) =>
        (l.adheresFilter(el) && l.adheresFilter(er)) || (r.adheresFilter(el) && r.adheresFilter(er))
    }
  }

  case class FilterElem(tag: String) extends FilterCond {
    def filterTags: Seq[String] = Seq(tag)

    def adheresFilter(elemFilter: FilterCond): Boolean = elemFilter match {
      case FilterElem(fTag) =>        tag == fTag
      case FilterCalc(l,r,OR) => this.adheresFilter(l) || this.adheresFilter(r)
      case FilterCalc(l,r,AND) => this.adheresFilter(l) && this.adheresFilter(r)


    }
  }

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

  def findPossibleTags(tagStr: SiteIdent): Seq[FilterTag] =
    filterTags.flatMap(_.findPossibleTags(tagStr))
      .take(10)

  def allTags(): Seq[String] =
    filterTags.flatMap(_.allTags())

}
