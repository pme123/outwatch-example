package nextds.server.control

import nextds.entity._

/**
  * Created by pascal.mengelt on 23.06.2017.
  */
object FilterTagCreator {

  import SitesRepo._

  lazy val filterTags =
    FilterTags(
      Seq(
        TagGroup(allSites.head, "Language")
          .addChildren("DE", "FR", "IT", "EN")
        , TagGroup(filtSite, "Store")
          .addChildren("MGA", "ORP", "CPF", "NJR", "AJR", "BJR")
      ))

  lazy val filterTagConfs =
    Seq(
      FilterTagConf(filtSite, "DE AND (MGA OR CPF)", Seq("DE", "MGA", "CPF").map(filterTags.filterTag))
      , FilterTagConf(filtSite, "(EN AND ORP) OR NJR", Seq("EN", "ORP", "NJR").map(filterTags.filterTag))
      , FilterTagConf(filtSite, "EN OR DE", Seq("EN", "DE").map(filterTags.filterTag))
      , FilterTagConf(filtSite, "EN AND DE", Seq("EN", "DE").map(filterTags.filterTag))
      , FilterTagConf(filtSite, "(IT OR EN) AND DE", Seq("EN", "DE", "IT").map(filterTags.filterTag))
      , FilterTagConf(filtSite, "EN", Seq("EN").map(filterTags.filterTag))
      , FilterTagConf(filtSite, "DE", Seq("DE").map(filterTags.filterTag))
      , FilterTagConf(filtSite, "FR", Seq("FR").map(filterTags.filterTag))
      , FilterTagConf(filtSite, "IT", Seq("IT").map(filterTags.filterTag))
    )

  def filterTagConf(condition:String): FilterTagConf =
    filterTagConfs.find(_.condition == condition).get

  import cats.data._
  import cats.implicits._
  import Validated.{valid, invalid}
  import cats.data.{NonEmptyList=>NEL}

  def filterTags(fc: FilterCond): Validated[NEL[String], NEL[FilterTag]] =
    fc.filterTags
      .map(ft => filterTags.filterTagOpt(ft) match {
        case None => invalid[NEL[String], NEL[FilterTag]](NEL.of(s"No Filter Tag found for $ft"))
        case Some(tag:FilterTag) => valid[NEL[String], NEL[FilterTag]](NEL.of(tag))
      }
      ).reduceLeft(_ combine _)



}

