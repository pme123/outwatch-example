import cats._
import cats.data._
import cats.implicits._
import Validated.{valid, invalid}
import cats.data.{NonEmptyList=>NEL}

val result =
  (valid[NEL[String], String]("event 1 ok") |@|
    invalid[NEL[String], String](NEL.of("event 2 failed!")) |@|
    invalid[NEL[String], String](NEL.of("event 3 failed!"))) map {_ + _ + _}
val empty: Validated[NEL[String],String] = valid("ok")

val result2 =
  Seq(valid[NEL[String], String]("event 1 ok") ,
    invalid[NEL[String], String](NEL.of("event 2 failed!")) ,
    invalid[NEL[String], String](NEL.of("event 3 failed!")))
    .reduceLeft(_ combine _)

