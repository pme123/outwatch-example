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
      FilterTagConf(filtSite, "DE AND (MGA OR CPF)", filterTags)
      , FilterTagConf(filtSite, "(EN AND ORP) OR NJR", filterTags)
      , FilterTagConf(filtSite, "EN OR DE", filterTags)
      , FilterTagConf(filtSite, "EN AND DE", filterTags)
      , FilterTagConf(filtSite, "(IT OR EN) AND DE", filterTags)
      , FilterTagConf(filtSite, "EN", filterTags)
      , FilterTagConf(filtSite, "DE", filterTags)
      , FilterTagConf(filtSite, "FR", filterTags)
      , FilterTagConf(filtSite, "IT", filterTags)
    )

  def filterTagConf(condition:String): FilterTagConf =
    filterTagConfs.find(_.condition == condition).get





}

