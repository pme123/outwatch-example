package nextds.server.control

import nextds.entity.{TimingConf, _}

/**
  * Created by pascal.mengelt on 27.06.2017.
  */
object TimingCreator {

  import SitesRepo._
  import TimingComp._

  lazy val timingComps =
    Seq(
      weekend(timerSite)
      , weekdays(timerSite)
      , saturday(timerSite)
      , sunday(timerSite)
      , monday(timerSite)
      , tuesday(timerSite)
      , wednesday(timerSite)
      , thursday(timerSite)
      , friday(timerSite)
      , TimingComp(timerSite, "09:00h-12:00h", TimePeriod("09:00", "12:00"))
      , TimingComp(timerSite, "14:00h-19:45h", TimePeriod("14:00", "19:45"))
      , TimingComp(timerSite, "24.12.2017 - 31.12.2017", DayPeriod("2017-12-24", "2017-12-31"))
    )

  lazy val timingConfs =
    Seq(
      TimingConf(timerSite, "Weekends", Seq(weekend(timerSite)))
      , TimingConf(timerSite, "Weekdays", Seq(weekdays(timerSite)))
      , TimingConf(timerSite, "Monday and Friday", Seq(monday(timerSite), friday(timerSite)))
      , TimingConf(timerSite, "Opening Hours", Seq(weekdays(timerSite), timingComps(9), timingComps(10)))
      , TimingConf(timerSite, "Christmas Week", Seq(timingComps.last))
    )

  def timingConf(title: String): TimingConf =
    timingConfs.find(_.title == title).get

  lazy val siteLevel: SiteLevel = SiteLevel(TIME, Map(
    TIMING -> SiteEntities(TIME, TIMING, timingConfs)
  ))
}
