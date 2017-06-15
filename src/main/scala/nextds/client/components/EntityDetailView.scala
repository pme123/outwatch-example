package nextds.client.components

import nextds.client.entity._
import nextds.entity.{LevelType, SiteType}
import outwatch.dom._

/**
  * Created by pascal.mengelt on 24.05.2017.
  */
object EntityDetailView {
  @inline private def bss = BootstrapStyles

  @inline private def gStyles = GlobalStyles

  object Style {

    def detailView(levelType: LevelType): String =
      Seq(
        gStyles.levelTypeStyle(levelType)
        , bss.panel.standard
      ) mkString " "

    def detailHeader(siteType: SiteType): String =
      gStyles.siteTypeStyle(siteType)


  }

  private def entityProps(uiEntity: UISiteEntity)(implicit store: ReduxStore[State, Action]): VNode = {
    val entity = uiEntity.siteEntity
    val handler = createHandler[Seq[VNode]]()
    div(className := Style.detailView(entity.levelType)
      , table(className := "table"
        , thead(className := Style.detailHeader(entity.siteType)
          , tr(th(className := UIElements.Style.labelCol
            , EntityCard.entityIcon(entity)
          )
            , th(className := UIElements.Style.valueCol
              , EntityCard.entityIdent(entity))
          ))
        , tbody(
          uiEntity.parameterEdit(): _*
        )
      )
    )
  }

  def apply()(implicit store: ReduxStore[State, Action]): VNode = {
    val setStream = store.map(_.selectedSET)
    div(className := bss.grid.col3
      , hidden <-- setStream.map(_.isEmpty)
      , child <-- setStream
        .map(_.map(entityProps)
          .getOrElse(""))
    )
  }
}

