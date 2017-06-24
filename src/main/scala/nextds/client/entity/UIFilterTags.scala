package nextds.client.entity

import nextds.client.entity.UIElements.Style
import nextds.entity._
import nextds.server.boundary.FilterTagBoundary
import outwatch.dom._

/**
  * Created by pascal.mengelt on 23.06.2017.
  */
case class UIFilterTags(filterTags: FilterTags
                        , filterTagConfs: Seq[UIFilterTagConf]) {
}

object UIFilterTags {
  def apply(filterTags: FilterTags = FilterTagBoundary.filterTags()): UIFilterTags =
    new UIFilterTags(filterTags
      , FilterTagBoundary.filterTagConfs()
        .map(UIFilterTagConf(_)))
}

case class UIFilterTagConf(siteEntity: FilterTagConf
                           , isFiltered: Boolean = false) extends UISiteEntity {
  val condition: String = siteEntity.condition

  val htmlCondition: VNode = condition.italic

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit() ++
      siteEntity.filterTags
        .map(ft =>
          editValue(siteType.label, div(
             tpe := "text"
            , ft.tag
            , Attribute("data-toggle", "tooltip")
            , Attribute("title", "TO DO")
            , disabled := true
          )))

  protected def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)

  // all links to the level CONF
  def withLinkedUp(uiModel: UISiteModel): Set[SiteEntityIdent] = {
    uiModel.level(CONF).siteEntities.values.flatten
      .filter(_.asInstanceOf[UISiteConf].filterTagConf.exists(_.ident == ident))
      .flatMap(_.withLinkedUp(uiModel))
      .toSet + ident
  }

  // no levels below
  def withLinkedDown(uiModel: UISiteModel): Set[SiteEntityIdent] = {
    Set(ident)
  }
}
