package nextds.entity

import scala.scalajs.js.JSON

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteTemplTrait
  extends SiteEntityTrait {
  def siteInfo: SiteEntityInfo

  lazy val levelType: LevelType = TEMPL

  // no upper levels
  def withLinkedUp(siteModel: SiteModel): Set[SiteEntityTrait] = {
    Set(this)
  }

  // all links to the level COMP
  def withLinkedDown(siteModel: SiteModel): Set[SiteEntityTrait] = {
    siteModel.entities(COMP, siteType)
      .filter(_.asInstanceOf[SiteCompTrait].templ.ident == ident)
      .flatMap(_.withLinkedDown(siteModel))
      .toSet + this
  }

}

object SiteTemplTrait {
}

case class PlayerTempl(siteInfo: SiteEntityInfo)
  extends SiteTemplTrait
    with PlayerTrait {
}

object PlayerTempl {

  def apply(siteIdent: String, title: String): PlayerTempl = PlayerTempl(SiteEntityInfo(siteIdent, Site.nextIdent(siteIdent), title))
}

case class LayoutTempl(siteInfo: SiteEntityInfo
                       , screenRegion: ScreenRegion)
  extends SiteTemplTrait
    with LayoutTrait {

  lazy val fromLeft: Int = screenRegion.fromLeft
  lazy val fromTop: Int = screenRegion.fromTop
  lazy val width: Int = screenRegion.width
  lazy val height: Int = screenRegion.height

  lazy val maybeScreenRegion: Option[ScreenRegion] = Some(screenRegion)
}

object LayoutTempl {

  def singleLayout(siteIdent: String
                   , title: String
                   , screenRegion: ScreenRegion): LayoutTempl =
    LayoutTempl(SiteEntityInfo(siteIdent, Site.nextIdent(siteIdent), title)
      , screenRegion)

  def singleLayout(templContent: SiteEntityInfo
                   , screenRegion: ScreenRegion): LayoutTempl =
    LayoutTempl(templContent
      , screenRegion)

}

case class PlaylistTempl(siteInfo: SiteEntityInfo)
  extends SiteTemplTrait
    with PlaylistTrait {
}

object PlaylistTempl {
}

case class MediumTempl(siteInfo: SiteEntityInfo, templConfig: Option[MediumTemplConfig] = None)
  extends SiteTemplTrait
    with MediumTrait {
}

object MediumTempl {
}

case class MediumTemplConfig(configValues: Seq[MediumTemplValue[_]])

object MediumTemplConfig {
}

case class ValueLabel(language: String, title: String, descr: Option[String])

case class MediumTemplValue[T](key: String
                               , valueType: ValueType[T]
                               , defaultValue: Option[T] = None
                               , labels: Seq[ValueLabel] = Nil)

trait ValueType[T] {
  def valueClass: Class[T]

  def outputValue(value: T): String

}

case object StringValueConfig
  extends ValueType[String] {
  val valueClass: Class[String] = classOf[String]

  def outputValue(value: String): String = value
}

case object BooleanValueConfig
  extends ValueType[Boolean] {
  val valueClass: Class[Boolean] = classOf[Boolean]

  def outputValue(value: Boolean): String = value.toString
}

case object MediumValueConfig
  extends ValueType[MediumRef] {
  val valueClass: Class[MediumRef] = classOf[MediumRef]

  def outputValue(value: MediumRef): String = value.url
}

case class MediumRef(ident: String, url: String)


























