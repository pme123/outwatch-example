package nextds.entity

/**
  * Created by pascal.mengelt on 27.06.2017.
  * it has no template - so it is a special Comp.
  */
case class TimingComp(siteInfo: SiteEntityInfo
                      , weekDays: WeekDays = WeekDays(),
                      play: Boolean = false,
                      dayPeriod: DayPeriod = DayPeriod(),
                      timePeriod: TimePeriod = TimePeriod()
                     ) extends SiteEntityTrait
  with TimeTrait {
}

object TimingComp {

  def apply(siteIdent: SiteIdent, title: String, weekDays: WeekDays): TimingComp =
    TimingComp(SiteEntityInfo(siteIdent, Site.nextIdent(siteIdent), title), weekDays)

  def apply(siteIdent: SiteIdent, title: String, dayPeriod: DayPeriod): TimingComp =
    TimingComp(SiteEntityInfo(siteIdent, Site.nextIdent(siteIdent), title), dayPeriod = dayPeriod)

  def apply(siteIdent: SiteIdent, title: String, timePeriod: TimePeriod): TimingComp =
    TimingComp(SiteEntityInfo(siteIdent, Site.nextIdent(siteIdent), title), timePeriod = timePeriod)

  def weekend(siteIdent: SiteIdent) = TimingComp(siteIdent, "Weekend", WeekDays.weekend)

  def weekdays(siteIdent: SiteIdent) = TimingComp(siteIdent, "Weekdays", WeekDays.weekdays)

  def saturday(siteIdent: SiteIdent) = TimingComp(siteIdent, "Saturday", WeekDays.saturday)

  def sunday(siteIdent: SiteIdent) = TimingComp(siteIdent, "Sunday", WeekDays.sunday)

  def monday(siteIdent: SiteIdent) = TimingComp(siteIdent, "Saturday", WeekDays.monday)

  def tuesday(siteIdent: SiteIdent) = TimingComp(siteIdent, "Tuesday", WeekDays.tuesday)

  def wednesday(siteIdent: SiteIdent) = TimingComp(siteIdent, "Wednesday", WeekDays.wednesday)

  def thursday(siteIdent: SiteIdent) = TimingComp(siteIdent, "Thursday", WeekDays.thursday)

  def friday(siteIdent: SiteIdent) = TimingComp(siteIdent, "Friday", WeekDays.friday)

}

case class TimingConf(siteInfo: SiteEntityInfo
                      , timingComps: Seq[TimingComp] = Nil) extends TimeTrait {

}

object TimingConf {
  def apply(siteIdent: String, title: String, timings: Seq[TimingComp]): TimingConf =
    TimingConf(SiteEntityInfo(siteIdent, Site.nextIdent(siteIdent), title), timings)

}


case class TimingComps(timingComps: Seq[TimingConf]) {

}
case class WeekDays(mon: Boolean = true, tue: Boolean = true, wed: Boolean = true,
                    thu: Boolean = true, fri: Boolean = true, sat: Boolean = true, sun: Boolean = true)

object WeekDays {
  def weekend = WeekDays(mon = false, tue = false, wed = false, thu = false, fri = false)

  def weekdays = WeekDays(sat = false, sun = false)

  def saturday = WeekDays(sat = true, sun = false, mon = false, tue = false, wed = false, thu = false, fri = false)

  def sunday = WeekDays(sat = false, sun = true, mon = false, tue = false, wed = false, thu = false, fri = false)

  def monday = WeekDays(sat = false, sun = false, mon = true, tue = false, wed = false, thu = false, fri = false)

  def tuesday = WeekDays(sat = false, sun = false, mon = false, tue = true, wed = false, thu = false, fri = false)

  def wednesday = WeekDays(sat = false, sun = false, mon = false, tue = false, wed = true, thu = false, fri = false)

  def thursday = WeekDays(sat = false, sun = false, mon = false, tue = false, wed = false, thu = true, fri = false)

  def friday = WeekDays(sat = false, sun = false, mon = false, tue = false, wed = false, thu = false, fri = true)
}

case class DayPeriod(from: Option[String] = None, to: Option[String] = None) {


}

object DayPeriod {
  val fromMustBeSmaller = "The from Day must be smaller or equal than the to Day!"

  def apply(fromDay: String, toDay: String): DayPeriod =
    DayPeriod(Some(fromDay), Some(toDay))
}


case class TimePeriod(from: Option[String] = None, to: Option[String] = None) {

}

object TimePeriod {
  def apply(fromStr: String, toStr: String): TimePeriod =
    TimePeriod(Some(fromStr), Some(toStr))

}
