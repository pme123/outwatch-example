package nextds.server.control

import nextds.entity._

/**
  * Created by pascal.mengelt on 23.06.2017.
  */
object FilterTagCreator {

  import SitesCreator._

  lazy val createFilterTags =
    FilterTags(
      Seq(
        TagGroup(allSites.head, "Language")
          .addChildren("DE", "FR", "IT", "EN")
        , TagGroup(allSites.last, "Store")
          .addChildren("MGA", "ORP", "CPF", "NJR", "AJR", "BJR")
      ))

  lazy val createFilterTagConfs =
    Seq(
      FilterTagConf(allSites.last, "DE AND (MGA OR CPF)", Seq("DE", "MGA", "CPF").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "(EN AND ORP) OR NJR", Seq("EN", "ORP", "NJR").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "EN OR DE", Seq("EN").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "EN", Seq("EN").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "DE", Seq("DE").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "FR", Seq("FR").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "IT", Seq("IT").map(createFilterTags.filterTag))
    )

  def filterTagConf(condition:String): FilterTagConf =
    createFilterTagConfs.find(_.condition == condition).get

  import cats.data._
  import cats.implicits._
  import Validated.{valid, invalid}
  import cats.data.{NonEmptyList=>NEL}

  def filterTags(fc: FilterCond): Validated[NEL[String], NEL[FilterTag]] =
    fc.filterTags
      .map(ft => createFilterTags.filterTagOpt(ft) match {
        case None => invalid[NEL[String], NEL[FilterTag]](NEL.of(s"No Filter Tag found for $ft"))
        case Some(tag:FilterTag) => valid[NEL[String], NEL[FilterTag]](NEL.of(tag))
      }
      ).reduceLeft(_ combine _)



}

