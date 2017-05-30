enablePlugins(ScalaJSPlugin)

name := "Outwatchexample"

version := "0.1.0"

organization := "pme"

scalaVersion := "2.12.1"

jsEnv := PhantomJSEnv().value


libraryDependencies ++= Seq(
  "io.github.outwatch" %%% "outwatch" % "0.9.2"
  , "com.github.japgolly.scalacss" %%% "core" % "0.5.1"
  , "org.scalatest" %%% "scalatest" % "3.0.1" % Test
)

jsDependencies ++= Seq(
  "org.webjars" % "jquery" % "1.11.1" / "jquery.js" minified "jquery.min.js"
  , "org.webjars" % "bootstrap" % "3.3.6" / "bootstrap.js" minified "bootstrap.min.js" dependsOn "jquery.js"
)